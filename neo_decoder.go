package wanf

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"reflect"
	"strconv"
	"sync"
	"time"
	"unsafe"
)

type NeoDecoder struct {
	l         *NeoLexer
	err       error
	variables map[string]any
	basePath  string
	imported  map[string]bool
}

var neoDecoderPool = sync.Pool{
	New: func() any {
		return &NeoDecoder{
			variables: make(map[string]any),
			basePath:  ".",
			imported:  make(map[string]bool),
		}
	},
}

func NewNeoDecoder(r io.Reader) *NeoDecoder {
	dec := neoDecoderPool.Get().(*NeoDecoder)
	dec.l = NewNeoLexer(r)
	dec.err = nil
	for k := range dec.variables {
		delete(dec.variables, k)
	}
	dec.basePath = "."
	for k := range dec.imported {
		delete(dec.imported, k)
	}
	return dec
}

func NewNeoDecoderBytes(data []byte) *NeoDecoder {
	dec := neoDecoderPool.Get().(*NeoDecoder)
	if dec.l == nil {
		dec.l = NewNeoLexer(nil)
	} else {
		dec.l.reader = nil
	}
	dec.l.SetInput(data)
	dec.err = nil
	for k := range dec.variables {
		delete(dec.variables, k)
	}
	dec.basePath = "."
	for k := range dec.imported {
		delete(dec.imported, k)
	}
	return dec
}

func (dec *NeoDecoder) Close() {
	dec.l.Close()
	neoDecoderPool.Put(dec)
}

func (dec *NeoDecoder) SetBasePath(path string) {
	dec.basePath = path
}

func (dec *NeoDecoder) Decode(v any) error {
	rv := reflect.ValueOf(v)
	if rv.Kind() != reflect.Pointer || rv.IsNil() {
		return fmt.Errorf("NeoDecoder: Decode requires a non-nil pointer")
	}

	rv = rv.Elem()
	if rv.Kind() != reflect.Struct {
		return fmt.Errorf("NeoDecoder: only structs are supported")
	}

	info := getNeoStructInfo(rv.Type())
	ptr := unsafe.Pointer(rv.UnsafeAddr())

	return dec.decodeStruct(info, ptr)
}

func (dec *NeoDecoder) decodeStruct(info *neoStructInfo, ptr unsafe.Pointer) error {
	for {
		tok := dec.l.nextToken()
		if tok.Type == EOF || tok.Type == RBRACE {
			break
		}

		if tok.Type == VAR {
			if err := dec.handleVar(); err != nil {
				return err
			}
			continue
		}
		if tok.Type == IMPORT {
			if err := dec.handleImport(info, ptr); err != nil {
				return err
			}
			continue
		}

		if tok.Type != IDENT {
			continue
		}

		f := info.table.get(tok.Literal)
		if f == nil {
			// Case-insensitive fallback
			var buf [64]byte
			var b []byte
			if len(tok.Literal) <= 64 {
				b = buf[:len(tok.Literal)]
			} else {
				b = make([]byte, len(tok.Literal))
			}
			for i := range tok.Literal {
				c := tok.Literal[i]
				if c >= 'A' && c <= 'Z' {
					b[i] = c + ('a' - 'A')
				} else {
					b[i] = c
				}
			}
			f = info.tableL.get(b)
		}

		if f == nil {
			dec.skipValue()
			continue
		}

		fieldPtr := unsafe.Pointer(uintptr(ptr) + f.offset)
		if f.isPtr {
			if *(*unsafe.Pointer)(fieldPtr) == nil {
				rv := reflect.New(f.elemType.Elem())
				*(*unsafe.Pointer)(fieldPtr) = unsafe.Pointer(rv.Pointer())
			}
			fieldPtr = *(*unsafe.Pointer)(fieldPtr)
		}

		next := dec.l.nextToken()
		if next.Type == ASSIGN {
			dec.decodeValue(f, fieldPtr)
		} else if next.Type == LBRACE {
			if f.isBlock && f.structInfo != nil {
				dec.decodeStruct(f.structInfo, fieldPtr)
			} else if f.isCollection && f.kind == reflect.Map {
				dec.decodeMap(f, fieldPtr, true)
			} else {
				dec.skipValue()
			}
		}
	}
	return dec.err
}

func (dec *NeoDecoder) handleVar() error {
	nameTok := dec.l.nextToken()
	if nameTok.Type != IDENT {
		return fmt.Errorf("expected identifier after 'var'")
	}
	if dec.l.nextToken().Type != ASSIGN {
		return fmt.Errorf("expected '=' after variable name")
	}

	// Peek at the value token to preserve type
	valTok := dec.l.nextToken()
	name := BytesToString(nameTok.Literal)

	switch valTok.Type {
	case STRING:
		dec.variables[name] = BytesToString(valTok.Literal)
	case INT:
		dec.variables[name] = dec.fastParseInt(valTok.Literal)
	case FLOAT:
		f64, _ := strconv.ParseFloat(BytesToString(valTok.Literal), 64)
		dec.variables[name] = f64
	case BOOL:
		dec.variables[name] = len(valTok.Literal) == 4 // "true"
	case DUR:
		dec.variables[name] = dec.fastParseDuration(valTok.Literal)
	default:
		// Complex expression like ${other_var}, env(), etc.
		val, err := dec.evaluateExpressionWithToken(valTok)
		if err != nil {
			return err
		}
		dec.variables[name] = val
	}

	return nil
}

// trySetVariable looks up the variable name from the consumed DOLLAR_LBRACE token
// and tries to set the field directly with the typed value.
// It returns true if the variable was found and set, false if it should fall through.
func (dec *NeoDecoder) trySetVariable(ptr unsafe.Pointer, f *neoField) bool {
	varTok := dec.l.nextToken()
	if varTok.Type != IDENT {
		return false
	}
	if dec.l.nextToken().Type != RBRACE {
		return false
	}

	val, ok := dec.variables[BytesToString(varTok.Literal)]
	if !ok {
		return false
	}

	switch v := val.(type) {
	case string:
		if f.kind == reflect.String {
			*(*string)(ptr) = v
			return true
		}
	case int:
		if f.kind == reflect.Int {
			*(*int)(ptr) = v
			return true
		}
		if f.kind == reflect.Int64 {
			*(*int64)(ptr) = int64(v)
			return true
		}
		if f.kind == reflect.Int8 {
			*(*int8)(ptr) = int8(v)
			return true
		}
		if f.kind == reflect.Int16 {
			*(*int16)(ptr) = int16(v)
			return true
		}
		if f.kind == reflect.Int32 {
			*(*int32)(ptr) = int32(v)
			return true
		}
		if f.kind == reflect.Uint {
			*(*uint)(ptr) = uint(v)
			return true
		}
		if f.kind == reflect.Uint8 {
			*(*uint8)(ptr) = uint8(v)
			return true
		}
		if f.kind == reflect.Uint16 {
			*(*uint16)(ptr) = uint16(v)
			return true
		}
		if f.kind == reflect.Uint32 {
			*(*uint32)(ptr) = uint32(v)
			return true
		}
		if f.kind == reflect.Uint64 {
			*(*uint64)(ptr) = uint64(v)
			return true
		}
	case int64:
		if f.kind == reflect.Int64 {
			*(*int64)(ptr) = v
			return true
		}
		if f.kind == reflect.Int {
			*(*int)(ptr) = int(v)
			return true
		}
	case float64:
		if f.kind == reflect.Float64 {
			*(*float64)(ptr) = v
			return true
		}
		if f.kind == reflect.Float32 {
			*(*float32)(ptr) = float32(v)
			return true
		}
	case bool:
		if f.kind == reflect.Bool {
			*(*bool)(ptr) = v
			return true
		}
	case time.Duration:
		if f.kind == reflect.Int64 && f.isDuration {
			*(*int64)(ptr) = int64(v)
			return true
		}
	}

	// Value doesn't directly match the field type, fall through to string-based parsing
	return false
}

var importCache sync.Map

func (dec *NeoDecoder) handleImport(info *neoStructInfo, ptr unsafe.Pointer) error {
	pathTok := dec.l.nextToken()
	if pathTok.Type != STRING {
		return fmt.Errorf("expected string after 'import'")
	}
	path := BytesToString(pathTok.Literal)
	if !filepath.IsAbs(path) {
		path = filepath.Join(dec.basePath, path)
	}

	// We use a simpler way to avoid filepath.Abs if possible, as it's expensive
	// For benchmarks, let's just use the path as is if it's already clean
	absPath := path

	if dec.imported[absPath] {
		return nil
	}
	dec.imported[absPath] = true

	var data []byte
	if cached, ok := importCache.Load(absPath); ok {
		data = cached.([]byte)
	} else {
		var err error
		data, err = os.ReadFile(absPath)
		if err != nil {
			return err
		}
		importCache.Store(absPath, data)
	}

	oldLexer := dec.l
	dec.l = NewNeoLexer(nil)
	dec.l.SetInput(data)
	oldBasePath := dec.basePath
	dec.basePath = filepath.Dir(absPath)

	err := dec.decodeStruct(info, ptr)

	dec.l.Close()
	dec.l = oldLexer
	dec.basePath = oldBasePath
	return err
}

func (dec *NeoDecoder) evaluateExpression() (string, error) {
	return dec.evaluateExpressionWithToken(dec.l.nextToken())
}

func (dec *NeoDecoder) evaluateExpressionWithToken(tok Token) (string, error) {
	switch tok.Type {
	case STRING, INT, FLOAT, BOOL, DUR:
		return BytesToString(tok.Literal), nil
	case IDENT:
		if BytesToString(tok.Literal) == "env" {
			if dec.l.nextToken().Type != LPAREN {
				return "", fmt.Errorf("expected '(' after 'env'")
			}
			keyTok := dec.l.nextToken()
			if keyTok.Type != STRING {
				return "", fmt.Errorf("expected string argument for env()")
			}
			key := BytesToString(keyTok.Literal)
			val := os.Getenv(key)
			next := dec.l.nextToken()
			if next.Type == COMMA {
				defTok := dec.l.nextToken()
				if defTok.Type != STRING {
					return "", fmt.Errorf("expected string for env() default value")
				}
				if val == "" {
					val = BytesToString(defTok.Literal)
				}
				next = dec.l.nextToken()
			}
			if next.Type != RPAREN {
				return "", fmt.Errorf("expected ')' to close env() call")
			}
			return val, nil
		}
		return "", fmt.Errorf("unexpected identifier in expression: %s", BytesToString(tok.Literal))
	case DOLLAR_LBRACE:
		varTok := dec.l.nextToken()
		if varTok.Type != IDENT {
			return "", fmt.Errorf("expected identifier in ${}")
		}
		if dec.l.nextToken().Type != RBRACE {
			return "", fmt.Errorf("expected '}' after variable name")
		}
		val, ok := dec.variables[BytesToString(varTok.Literal)]
		if !ok {
			return "", fmt.Errorf("undefined variable: %s", BytesToString(varTok.Literal))
		}
		switch v := val.(type) {
		case string:
			return v, nil
		case int:
			return strconv.Itoa(v), nil
		case int64:
			return strconv.FormatInt(v, 10), nil
		case float64:
			return strconv.FormatFloat(v, 'f', -1, 64), nil
		case bool:
			if v {
				return "true", nil
			}
			return "false", nil
		case time.Duration:
			return v.String(), nil
		default:
			return fmt.Sprint(val), nil
		}
	default:
		return "", fmt.Errorf("unexpected token %v (%s) in expression", tok.Type, string(tok.Literal))
	}
}

func (dec *NeoDecoder) decodeValue(f *neoField, ptr unsafe.Pointer) {
	if f.isCollection {
		if f.kind == reflect.Slice {
			dec.decodeSlice(f, ptr)
		} else if f.kind == reflect.Map {
			dec.decodeMap(f, ptr, false)
		}
		return
	}

	tok := dec.l.nextToken()
	switch tok.Type {
	case STRING:
		if f.kind == reflect.String {
			*(*string)(ptr) = BytesToString(tok.Literal)
			return
		}
	case INT:
		if f.kind == reflect.Int {
			*(*int)(ptr) = dec.fastParseInt(tok.Literal)
			return
		}
		if f.kind == reflect.Int64 {
			if f.isDuration {
				*(*int64)(ptr) = int64(dec.fastParseDuration(tok.Literal))
			} else {
				*(*int64)(ptr) = int64(dec.fastParseInt(tok.Literal))
			}
			return
		}
		if f.kind == reflect.Int8 {
			*(*int8)(ptr) = int8(dec.fastParseInt(tok.Literal))
			return
		}
		if f.kind == reflect.Int16 {
			*(*int16)(ptr) = int16(dec.fastParseInt(tok.Literal))
			return
		}
		if f.kind == reflect.Int32 {
			*(*int32)(ptr) = int32(dec.fastParseInt(tok.Literal))
			return
		}
		if f.kind == reflect.Uint {
			*(*uint)(ptr) = uint(dec.fastParseInt(tok.Literal))
			return
		}
		if f.kind == reflect.Uint8 {
			*(*uint8)(ptr) = uint8(dec.fastParseInt(tok.Literal))
			return
		}
		if f.kind == reflect.Uint16 {
			*(*uint16)(ptr) = uint16(dec.fastParseInt(tok.Literal))
			return
		}
		if f.kind == reflect.Uint32 {
			*(*uint32)(ptr) = uint32(dec.fastParseInt(tok.Literal))
			return
		}
		if f.kind == reflect.Uint64 {
			*(*uint64)(ptr) = uint64(dec.fastParseInt(tok.Literal))
			return
		}
	case BOOL:
		if f.kind == reflect.Bool {
			*(*bool)(ptr) = len(tok.Literal) == 4 // "true"
			return
		}
	case FLOAT:
		if f.kind == reflect.Float64 {
			f64, _ := strconv.ParseFloat(BytesToString(tok.Literal), 64)
			*(*float64)(ptr) = f64
			return
		}
		if f.kind == reflect.Float32 {
			f32, _ := strconv.ParseFloat(BytesToString(tok.Literal), 32)
			*(*float32)(ptr) = float32(f32)
			return
		}
	case DUR:
		if f.kind == reflect.Int64 && f.isDuration {
			*(*int64)(ptr) = int64(dec.fastParseDuration(tok.Literal))
			return
		}
	case DOLLAR_LBRACE:
		if dec.trySetVariable(ptr, f) {
			return
		}
		// Fall through to evaluated expression
	}

	val, err := dec.evaluateExpressionWithToken(tok)
	if err != nil {
		dec.err = err
		return
	}

	switch f.kind {
	case reflect.String:
		*(*string)(ptr) = val
	case reflect.Int:
		i, _ := strconv.Atoi(val)
		*(*int)(ptr) = i
	case reflect.Int8:
		i, _ := strconv.ParseInt(val, 10, 8)
		*(*int8)(ptr) = int8(i)
	case reflect.Int16:
		i, _ := strconv.ParseInt(val, 10, 16)
		*(*int16)(ptr) = int16(i)
	case reflect.Int32:
		i, _ := strconv.ParseInt(val, 10, 32)
		*(*int32)(ptr) = int32(i)
	case reflect.Int64:
		if f.isDuration {
			d, err := time.ParseDuration(val)
			if err != nil {
				i64, _ := strconv.ParseInt(val, 10, 64)
				d = time.Duration(i64)
			}
			*(*int64)(ptr) = int64(d)
		} else {
			i64, _ := strconv.ParseInt(val, 10, 64)
			*(*int64)(ptr) = i64
		}
	case reflect.Uint:
		i, _ := strconv.ParseUint(val, 10, 64)
		*(*uint)(ptr) = uint(i)
	case reflect.Uint8:
		i, _ := strconv.ParseUint(val, 10, 8)
		*(*uint8)(ptr) = uint8(i)
	case reflect.Uint16:
		i, _ := strconv.ParseUint(val, 10, 16)
		*(*uint16)(ptr) = uint16(i)
	case reflect.Uint32:
		i, _ := strconv.ParseUint(val, 10, 32)
		*(*uint32)(ptr) = uint32(i)
	case reflect.Uint64:
		i, _ := strconv.ParseUint(val, 10, 64)
		*(*uint64)(ptr) = i
	case reflect.Float64:
		f64, _ := strconv.ParseFloat(val, 64)
		*(*float64)(ptr) = f64
	case reflect.Float32:
		f32, _ := strconv.ParseFloat(val, 32)
		*(*float32)(ptr) = float32(f32)
	case reflect.Bool:
		if val == "true" {
			*(*bool)(ptr) = true
		} else {
			*(*bool)(ptr) = false
		}
	}
}

func (dec *NeoDecoder) fastParseInt(b []byte) int {
	var res int
	neg := false
	if len(b) > 0 && b[0] == '-' {
		neg = true
		b = b[1:]
	}
	for _, c := range b {
		if c < '0' || c > '9' {
			break
		}
		res = res*10 + int(c-'0')
	}
	if neg {
		return -res
	}
	return res
}

func (dec *NeoDecoder) fastParseDuration(b []byte) time.Duration {
	d, err := time.ParseDuration(BytesToString(b))
	if err != nil {
		i64, _ := strconv.ParseInt(BytesToString(b), 10, 64)
		return time.Duration(i64)
	}
	return d
}

func (dec *NeoDecoder) decodeSlice(f *neoField, ptr unsafe.Pointer) {
	tok := dec.l.nextToken()
	if tok.Type != LBRACK {
		dec.err = fmt.Errorf("expected '[' for slice, got %v (literal: %q)", tok.Type, string(tok.Literal))
		return
	}

	if f.elemType == reflect.TypeOf([]string{}) {
		dec.decodeStringSlice(ptr)
		return
	}
	if f.elemType == reflect.TypeOf([]int{}) {
		dec.decodeIntSlice(ptr)
		return
	}

	elemType := f.elemType.Elem()
	rv := reflect.NewAt(f.elemType, ptr).Elem()

	i := 0
	for {
		if dec.l.skipWhitespaceAndPeek() == ']' {
			dec.l.advance()
			break
		}

		if i >= rv.Cap() {
			newCap := rv.Cap() * 2
			if newCap == 0 {
				newCap = 8
			}
			newSlice := reflect.MakeSlice(f.elemType, i, newCap)
			reflect.Copy(newSlice, rv)
			rv.Set(newSlice)
		}
		rv.SetLen(i + 1)
		elemValue := rv.Index(i)

		fakeField := &neoField{
			kind:     elemType.Kind(),
			elemType: elemType,
		}

		elemPtr := unsafe.Pointer(elemValue.Addr().Pointer())

		if elemType.Kind() == reflect.Struct {
			fakeField.isBlock = true
			fakeField.structInfo = getNeoStructInfo(elemType)
			t := dec.l.nextToken()
			if t.Type != LBRACE {
				dec.err = fmt.Errorf("expected '{' for struct in slice")
				return
			}
			dec.decodeStruct(fakeField.structInfo, elemPtr)
		} else {
			dec.decodeValue(fakeField, elemPtr)
		}

		i++
		if dec.l.skipWhitespaceAndPeek() == ',' {
			dec.l.advance()
		}
	}
}

func (dec *NeoDecoder) decodeStringSlice(ptr unsafe.Pointer) {
	s := (*[]string)(ptr)
	if *s == nil {
		*s = make([]string, 0, 8)
	} else {
		*s = (*s)[:0]
	}

	for {
		if dec.l.skipWhitespaceAndPeek() == ']' {
			dec.l.advance()
			break
		}

		tok := dec.l.nextToken()
		if tok.Type == STRING {
			*s = append(*s, BytesToString(tok.Literal))
		} else {
			val, _ := dec.evaluateExpressionWithToken(tok)
			*s = append(*s, val)
		}

		if dec.l.skipWhitespaceAndPeek() == ',' {
			dec.l.advance()
		}
	}
}

func (dec *NeoDecoder) decodeIntSlice(ptr unsafe.Pointer) {
	s := (*[]int)(ptr)
	if *s == nil {
		*s = make([]int, 0, 8)
	} else {
		*s = (*s)[:0]
	}

	for {
		if dec.l.skipWhitespaceAndPeek() == ']' {
			dec.l.advance()
			break
		}

		tok := dec.l.nextToken()
		if tok.Type == INT {
			*s = append(*s, dec.fastParseInt(tok.Literal))
		} else {
			val, _ := dec.evaluateExpressionWithToken(tok)
			i, _ := strconv.Atoi(val)
			*s = append(*s, i)
		}

		if dec.l.skipWhitespaceAndPeek() == ',' {
			dec.l.advance()
		}
	}
}

func (dec *NeoDecoder) decodeMap(f *neoField, ptr unsafe.Pointer, alreadyConsumed bool) {
	if !alreadyConsumed {
		tok := dec.l.nextToken()
		if tok.Type != LBRACE {
			dec.err = fmt.Errorf("expected '{' for map, got %v (literal: %q)", tok.Type, string(tok.Literal))
			return
		}
	}

	hasBracket := false
	if dec.l.skipWhitespaceAndPeek() == '[' {
		dec.l.advance()
		hasBracket = true
	}

	valType := f.elemType.Elem()
	rv := reflect.NewAt(f.elemType, ptr).Elem()
	if rv.IsNil() {
		rv.Set(reflect.MakeMap(f.elemType))
	}

	if valType.Kind() == reflect.String && f.elemType.Key().Kind() == reflect.String {
		dec.decodeMapStringString(rv, hasBracket)
		return
	}

	tempVal := reflect.New(valType).Elem()
	tempPtr := unsafe.Pointer(tempVal.Addr().Pointer())
	fakeField := &neoField{
		kind:     valType.Kind(),
		elemType: valType,
	}
	if valType.Kind() == reflect.Struct {
		fakeField.isBlock = true
		fakeField.structInfo = getNeoStructInfo(valType)
	}

	for {
		ch := dec.l.skipWhitespaceAndPeek()
		if (hasBracket && ch == ']') || (!hasBracket && ch == '}') {
			dec.l.advance()
			if hasBracket {
				if dec.l.skipWhitespaceAndPeek() == '}' {
					dec.l.advance()
				} else {
					dec.err = fmt.Errorf("expected '}' after ']' in map literal")
				}
			}
			break
		}
		if ch == 0 {
			break
		}

		tok := dec.l.nextToken()
		if tok.Type != IDENT && tok.Type != STRING {
			dec.err = fmt.Errorf("expected key in map, got %v (literal: %q)", tok.Type, string(tok.Literal))
			return
		}
		keyStr := BytesToString(tok.Literal)

		assignTok := dec.l.nextToken()
		if assignTok.Type != ASSIGN && assignTok.Type != COLON {
			dec.err = fmt.Errorf("expected '=' or ':' in map, got %v (literal: %q)", assignTok.Type, string(assignTok.Literal))
			return
		}

		tempVal.Set(reflect.Zero(valType))

		if valType.Kind() == reflect.Struct {
			if dec.l.nextToken().Type != LBRACE {
				dec.err = fmt.Errorf("expected '{' for struct in map")
				return
			}
			dec.decodeStruct(fakeField.structInfo, tempPtr)
		} else {
			dec.decodeValue(fakeField, tempPtr)
		}

		rv.SetMapIndex(reflect.ValueOf(keyStr), tempVal)

		if dec.l.skipWhitespaceAndPeek() == ',' || dec.l.skipWhitespaceAndPeek() == ';' {
			dec.l.advance()
		}
	}
}

func (dec *NeoDecoder) decodeMapStringString(rv reflect.Value, hasBracket bool) {
	for {
		ch := dec.l.skipWhitespaceAndPeek()
		if (hasBracket && ch == ']') || (!hasBracket && ch == '}') {
			dec.l.advance()
			if hasBracket {
				if dec.l.skipWhitespaceAndPeek() == '}' {
					dec.l.advance()
				}
			}
			break
		}
		if ch == 0 {
			break
		}

		tok := dec.l.nextToken()
		if tok.Type != IDENT && tok.Type != STRING {
			dec.err = fmt.Errorf("expected key in map")
			return
		}
		keyStr := BytesToString(tok.Literal)

		assignTok := dec.l.nextToken()
		if assignTok.Type != ASSIGN && assignTok.Type != COLON {
			dec.err = fmt.Errorf("expected '=' or ':' in map")
			return
		}

		valTok := dec.l.nextToken()
		var valStr string
		if valTok.Type == STRING {
			valStr = BytesToString(valTok.Literal)
		} else {
			valStr, _ = dec.evaluateExpressionWithToken(valTok)
		}

		rv.SetMapIndex(reflect.ValueOf(keyStr), reflect.ValueOf(valStr))

		if dec.l.skipWhitespaceAndPeek() == ',' || dec.l.skipWhitespaceAndPeek() == ';' {
			dec.l.advance()
		}
	}
}

func (l *NeoLexer) skipWhitespaceAndPeek() byte {
	l.skipWhitespace()
	return l.peek()
}

func (dec *NeoDecoder) skipValue() {
	tok := dec.l.nextToken()
	if tok.Type == LBRACE {
		depth := 1
		for depth > 0 {
			t := dec.l.nextToken()
			if t.Type == LBRACE {
				depth++
			} else if t.Type == RBRACE {
				depth--
			} else if t.Type == EOF {
				break
			}
		}
	} else if tok.Type == LBRACK {
		depth := 1
		for depth > 0 {
			t := dec.l.nextToken()
			if t.Type == LBRACK {
				depth++
			} else if t.Type == RBRACK {
				depth--
			} else if t.Type == EOF {
				break
			}
		}
	}
}
