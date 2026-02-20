package wanf

import (
	"bufio"
	"bytes"
	"fmt"
	"io"
	"reflect"
	"sort"
	"strconv"
	"strings"
	"sync"
	"time"
	"unicode/utf8"
)

var encoderPool = sync.Pool{
	New: func() any {
		return &internalEncoder{
			buf: &bytes.Buffer{},
		}
	},
}

// fieldCache caches processed field information for a given struct type.
var fieldCache sync.Map // map[reflect.Type]*cachedStructInfo

var byteSlicePool = sync.Pool{
	New: func() any {
		b := make([]byte, 0, 64) // For strconv formatting
		return &b
	},
}

type mapEntry struct {
	key    reflect.Value
	keyStr string
	value  reflect.Value
}

var mapEntrySlicePool = sync.Pool{
	New: func() any {
		s := make([]mapEntry, 0, 16) // Start with capacity for 16 map entries
		return &s
	},
}

var tabs = []byte("\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t") // 16 tabs

var stringSlicePool = sync.Pool{
	New: func() any {
		s := make([]string, 0, 8)
		return &s
	},
}

var streamEncoderPool = sync.Pool{
	New: func() any {
		return &streamInternalEncoder{}
	},
}

var bufioWriterPool = sync.Pool{
	New: func() any {
		return bufio.NewWriter(io.Discard)
	},
}

func getEncoder() *internalEncoder {
	return encoderPool.Get().(*internalEncoder)
}

const maxPoolSliceCap = 1024

func putEncoder(e *internalEncoder) {
	if e.buf.Cap() > maxPoolSliceCap {
		e.buf = &bytes.Buffer{}
	} else {
		e.buf.Reset()
	}
	e.indent = 0
	encoderPool.Put(e)
}

func Marshal(v any) ([]byte, error) {
	e := getEncoder()
	defer putEncoder(e)

	e.opts = FormatOptions{
		Style:      StyleBlockSorted,
		EmptyLines: true,
	}

	tmpBufPtr := byteSlicePool.Get().(*[]byte)
	e.tmpBuf = *tmpBufPtr
	defer func() {
		*tmpBufPtr = (*tmpBufPtr)[:0]
		byteSlicePool.Put(tmpBufPtr)
	}()

	rv := reflect.ValueOf(v)
	if rv.Kind() == reflect.Pointer {
		rv = rv.Elem()
	}
	if !rv.IsValid() {
		return nil, fmt.Errorf("wanf: can only encode valid values")
	}

	if rv.Kind() == reflect.Struct {
		if err := e.encodeStruct(rv, 0); err != nil {
			return nil, err
		}
	} else {
		e.encodeValue(rv, 0)
	}

	if e.opts.Style != StyleSingleLine && e.buf.Len() > 0 {
		e.buf.WriteString("\n")
	}

	res := make([]byte, e.buf.Len())
	copy(res, e.buf.Bytes())
	return res, nil
}

type EncoderOption func(*FormatOptions)

func WithStyle(style OutputStyle) EncoderOption {
	return func(o *FormatOptions) {
		o.Style = style
	}
}

func WithoutEmptyLines() EncoderOption {
	return func(o *FormatOptions) {
		o.EmptyLines = false
	}
}

type Encoder struct {
	w io.Writer
	e *internalEncoder
}

func NewEncoder(w io.Writer, opts ...EncoderOption) *Encoder {
	options := FormatOptions{
		Style:      StyleBlockSorted,
		EmptyLines: true,
	}
	for _, opt := range opts {
		opt(&options)
	}
	e := getEncoder()
	e.opts = options
	return &Encoder{w: w, e: e}
}

func (enc *Encoder) Encode(v any) error {
	defer putEncoder(enc.e)

	tmpBufPtr := byteSlicePool.Get().(*[]byte)
	enc.e.tmpBuf = *tmpBufPtr
	defer func() {
		*tmpBufPtr = (*tmpBufPtr)[:0]
		byteSlicePool.Put(tmpBufPtr)
	}()

	rv := reflect.ValueOf(v)
	if rv.Kind() == reflect.Pointer {
		rv = rv.Elem()
	}
	if !rv.IsValid() {
		return fmt.Errorf("wanf: can only encode valid values")
	}
	if rv.Kind() == reflect.Struct {
		if err := enc.e.encodeStruct(rv, 0); err != nil {
			return err
		}
	} else {
		enc.e.encodeValue(rv, 0)
	}
	if enc.e.opts.Style != StyleSingleLine && enc.e.buf.Len() > 0 {
		enc.e.buf.WriteString("\n")
	}
	_, err := enc.w.Write(enc.e.buf.Bytes())
	return err
}

type internalEncoder struct {
	buf    *bytes.Buffer
	indent int
	opts   FormatOptions
	tmpBuf []byte
}

type cachedField struct {
	name        string
	nameBytes   []byte
	tag         wanfTag
	fieldType   reflect.StructField
	isBlock     bool
	isBlockLike bool
	index       int
	kind        reflect.Kind
	isCollection bool
}

type cachedStructInfo struct {
	original []cachedField
	sorted   []cachedField
}

func (e *internalEncoder) encodeStruct(v reflect.Value, depth int) error {
	t := v.Type()
	cached, ok := fieldCache.Load(t)
	if !ok {
		cached = cacheStructInfo(t)
		fieldCache.Store(t, cached)
	}
	info := cached.(*cachedStructInfo)

	var cachedFields []cachedField
	if e.opts.NoSort {
		cachedFields = info.original
	} else {
		switch e.opts.Style {
		case StyleAllSorted:
			cachedFields = info.sorted
		case StyleBlockSorted:
			if depth > 0 {
				cachedFields = info.sorted
			} else {
				cachedFields = info.original
			}
		default:
			cachedFields = info.original
		}
	}

	var prevWasBlockLike bool
	var first = true
	for _, cf := range cachedFields {
		fv := v.Field(cf.index)
		val := fv
		if val.Kind() == reflect.Pointer {
			if val.IsNil() {
				continue
			}
			val = val.Elem()
		}

		if cf.tag.Omitempty {
			if (cf.isCollection && val.Len() == 0) || (!cf.isCollection && val.IsZero()) {
				continue
			}
		} else if cf.kind == reflect.Map && val.Len() == 0 {
			continue
		}

		e.writeSeparator(!first, cf.isBlockLike, prevWasBlockLike, depth)
		e.encodeField(cf, v, depth)
		prevWasBlockLike = cf.isBlockLike
		first = false
	}

	return nil
}

func (e *internalEncoder) encodeField(cf cachedField, v reflect.Value, depth int) {
	e.writeIndent()
	e.buf.Write(cf.nameBytes)
	e.writeSpace()

	fv := v.Field(cf.index)
	for fv.Kind() == reflect.Pointer {
		if fv.IsNil() {
			return
		}
		fv = fv.Elem()
	}
	if cf.isBlock {
		if fv.Kind() == reflect.Map {
			e.encodeMap(fv, depth+1)
		} else {
			e.buf.WriteString("{")
			e.writeNewLine()
			e.indent++
			e.encodeStruct(fv, depth+1)
			e.indent--
			e.writeNewLine()
			e.writeIndent()
			e.buf.WriteString("}")
		}
	} else {
		e.buf.WriteString("=")
		e.writeSpace()
		e.encodeValue(fv, depth)
	}
}

func (e *internalEncoder) encodeValue(v reflect.Value, depth int) {
	if !v.IsValid() {
		return
	}

	// 优先处理 interface{} 类型，使用 switch 枚举提高性能
	if v.Kind() == reflect.Interface {
		if v.IsNil() {
			return
		}
		e.encodeInterface(v.Interface(), depth)
		return
	}

	for v.Kind() == reflect.Pointer {
		if v.IsNil() {
			return
		}
		v = v.Elem()
	}

	if v.Type() == durationType {
		e.buf.WriteString(time.Duration(v.Int()).String())
		return
	}

	switch v.Kind() {
	case reflect.String:
		s := v.String()
		if e.opts.Style != StyleSingleLine && strings.Contains(s, "\n") {
			e.buf.WriteByte('`')
			e.buf.WriteString(s)
			e.buf.WriteByte('`')
		} else {
			e.writeQuotedString(s)
		}
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		e.buf.Write(strconv.AppendInt(e.tmpBuf[:0], v.Int(), 10))
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		e.buf.Write(strconv.AppendUint(e.tmpBuf[:0], v.Uint(), 10))
	case reflect.Float32, reflect.Float64:
		e.buf.Write(strconv.AppendFloat(e.tmpBuf[:0], v.Float(), 'f', -1, 64))
	case reflect.Bool:
		e.buf.Write(strconv.AppendBool(e.tmpBuf[:0], v.Bool()))
	case reflect.Slice, reflect.Array:
		e.encodeSlice(v, depth)
	case reflect.Struct:
		if v.NumField() == 0 {
			e.buf.WriteString("{}")
			return
		}
		e.buf.WriteString("{")
		e.writeNewLine()
		e.indent++
		e.encodeStruct(v, depth+1)
		e.indent--
		e.writeNewLine()
		e.writeIndent()
		e.buf.WriteString("}")
	case reflect.Map:
		if v.Type() == mapStringAnyType {
			e.encodeMapInterface(v.Interface().(map[string]any), depth)
			return
		}
		if v.Type() == mapStringStringType {
			e.encodeMapStringString(v.Interface().(map[string]string), depth)
			return
		}
		e.encodeMap(v, depth)
	}
}

func (e *internalEncoder) encodeSlice(v reflect.Value, depth int) {
	e.buf.WriteString("[")
	l := v.Len()
	if l == 0 {
		e.buf.WriteString("]")
		return
	}

	if e.opts.Style == StyleSingleLine {
		for i := 0; i < l; i++ {
			if i > 0 {
				e.buf.WriteString(",")
			}
			e.encodeValue(v.Index(i), depth)
		}
	} else {
		e.writeNewLine()
		e.indent++
		for i := 0; i < l; i++ {
			e.writeIndent()
			e.encodeValue(v.Index(i), depth)
			e.buf.WriteString(",")
			e.writeNewLine()
		}
		e.indent--
		e.writeIndent()
	}
	e.buf.WriteString("]")
}

func (e *internalEncoder) encodeInterface(i any, depth int) {
	if i == nil {
		return
	}

	switch val := i.(type) {
	case string:
		if e.opts.Style != StyleSingleLine && strings.Contains(val, "\n") {
			e.buf.WriteByte('`')
			e.buf.WriteString(val)
			e.buf.WriteByte('`')
		} else {
			e.writeQuotedString(val)
		}
	case int:
		e.buf.Write(strconv.AppendInt(e.tmpBuf[:0], int64(val), 10))
	case int64:
		e.buf.Write(strconv.AppendInt(e.tmpBuf[:0], val, 10))
	case int32:
		e.buf.Write(strconv.AppendInt(e.tmpBuf[:0], int64(val), 10))
	case uint:
		e.buf.Write(strconv.AppendUint(e.tmpBuf[:0], uint64(val), 10))
	case uint64:
		e.buf.Write(strconv.AppendUint(e.tmpBuf[:0], val, 10))
	case uint32:
		e.buf.Write(strconv.AppendUint(e.tmpBuf[:0], uint64(val), 10))
	case float64:
		e.buf.Write(strconv.AppendFloat(e.tmpBuf[:0], val, 'f', -1, 64))
	case float32:
		e.buf.Write(strconv.AppendFloat(e.tmpBuf[:0], float64(val), 'f', -1, 32))
	case bool:
		e.buf.Write(strconv.AppendBool(e.tmpBuf[:0], val))
	case time.Duration:
		e.buf.WriteString(val.String())
	case map[string]any:
		e.encodeMapInterface(val, depth)
	case map[string]string:
		e.encodeMapStringString(val, depth)
	case []any:
		e.encodeSliceInterface(val, depth)
	case []string:
		e.encodeSliceString(val, depth)
	case int8:
		e.buf.Write(strconv.AppendInt(e.tmpBuf[:0], int64(val), 10))
	case int16:
		e.buf.Write(strconv.AppendInt(e.tmpBuf[:0], int64(val), 10))
	case uint8:
		e.buf.Write(strconv.AppendUint(e.tmpBuf[:0], uint64(val), 10))
	case uint16:
		e.buf.Write(strconv.AppendUint(e.tmpBuf[:0], uint64(val), 10))
	default:
		// 回退到反射
		e.encodeValue(reflect.ValueOf(i), depth)
	}
}

func (e *internalEncoder) encodeMapStringString(m map[string]string, depth int) {
	e.buf.WriteString("{[")
	if len(m) == 0 {
		e.buf.WriteString("]}")
		return
	}

	keysPtr := stringSlicePool.Get().(*[]string)
	keys := (*keysPtr)[:0]
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	if e.opts.Style == StyleSingleLine {
		for i, k := range keys {
			if i > 0 {
				e.buf.WriteString(",")
			}
			e.buf.Write(StringToBytes(k))
			e.buf.WriteString("=")
			e.writeQuotedString(m[k])
		}
	} else {
		e.buf.WriteString("\n")
		e.indent++
		for _, k := range keys {
			e.writeIndent()
			e.buf.Write(StringToBytes(k))
			e.writeSpace()
			e.buf.WriteString("=")
			e.writeSpace()
			e.writeQuotedString(m[k])
			e.buf.WriteString(",")
			e.buf.WriteString("\n")
		}
		e.indent--
		e.writeIndent()
	}
	e.buf.WriteString("]}")
	if cap(keys) <= maxPoolSliceCap {
		*keysPtr = keys
		stringSlicePool.Put(keysPtr)
	}
}

func (e *internalEncoder) encodeSliceString(s []string, depth int) {
	e.buf.WriteString("[")
	l := len(s)
	if l == 0 {
		e.buf.WriteString("]")
		return
	}

	if e.opts.Style == StyleSingleLine {
		for i, v := range s {
			if i > 0 {
				e.buf.WriteString(",")
			}
			e.writeQuotedString(v)
		}
	} else {
		e.writeNewLine()
		e.indent++
		for _, v := range s {
			e.writeIndent()
			e.writeQuotedString(v)
			e.buf.WriteString(",")
			e.writeNewLine()
		}
		e.indent--
		e.writeIndent()
	}
	e.buf.WriteString("]")
}

func (e *internalEncoder) encodeMapInterface(m map[string]any, depth int) {
	e.buf.WriteString("{[")
	if len(m) == 0 {
		e.buf.WriteString("]}")
		return
	}

	keysPtr := stringSlicePool.Get().(*[]string)
	keys := (*keysPtr)[:0]
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	if e.opts.Style == StyleSingleLine {
		for i, k := range keys {
			if i > 0 {
				e.buf.WriteString(",")
			}
			e.buf.Write(StringToBytes(k))
			e.buf.WriteString("=")
			e.encodeInterface(m[k], depth)
		}
	} else {
		e.buf.WriteString("\n")
		e.indent++
		for _, k := range keys {
			e.writeIndent()
			e.buf.Write(StringToBytes(k))
			e.writeSpace()
			e.buf.WriteString("=")
			e.writeSpace()
			e.encodeInterface(m[k], depth)
			e.buf.WriteString(",")
			e.buf.WriteString("\n")
		}
		e.indent--
		e.writeIndent()
	}
	e.buf.WriteString("]}")
	if cap(keys) <= maxPoolSliceCap {
		*keysPtr = keys
		stringSlicePool.Put(keysPtr)
	}
}

func (e *internalEncoder) encodeSliceInterface(s []any, depth int) {
	e.buf.WriteString("[")
	l := len(s)
	if l == 0 {
		e.buf.WriteString("]")
		return
	}

	if e.opts.Style == StyleSingleLine {
		for i, v := range s {
			if i > 0 {
				e.buf.WriteString(",")
			}
			e.encodeInterface(v, depth)
		}
	} else {
		e.writeNewLine()
		e.indent++
		for _, v := range s {
			e.writeIndent()
			e.encodeInterface(v, depth)
			e.buf.WriteString(",")
			e.writeNewLine()
		}
		e.indent--
		e.writeIndent()
	}
	e.buf.WriteString("]")
}

func quickSortMapEntries(entries []mapEntry) {
	if len(entries) < 2 {
		return
	}
	pivot := entries[len(entries)/2].keyStr
	left, right := 0, len(entries)-1
	for left <= right {
		for entries[left].keyStr < pivot {
			left++
		}
		for entries[right].keyStr > pivot {
			right--
		}
		if left <= right {
			entries[left], entries[right] = entries[right], entries[left]
			left++
			right--
		}
	}
	if right > 0 {
		quickSortMapEntries(entries[:right+1])
	}
	if left < len(entries) {
		quickSortMapEntries(entries[left:])
	}
}

func (e *internalEncoder) encodeMap(v reflect.Value, depth int) {
	e.buf.WriteString("{[")
	l := v.Len()
	if l == 0 {
		e.buf.WriteString("]}")
		return
	}

	entriesPtr := mapEntrySlicePool.Get().(*[]mapEntry)
	entries := *entriesPtr
	if cap(entries) < l {
		entries = make([]mapEntry, 0, l)
	}
	entries = entries[:0]

	iter := v.MapRange()
	for iter.Next() {
		k := iter.Key()
		entries = append(entries, mapEntry{key: k, keyStr: k.String(), value: iter.Value()})
	}

	if len(entries) > 1 {
		quickSortMapEntries(entries)
	}

	if e.opts.Style == StyleSingleLine {
		for i, entry := range entries {
			if i > 0 {
				e.buf.WriteString(",")
			}
			e.buf.Write(StringToBytes(entry.keyStr))
			e.buf.WriteString("=")
			e.encodeValue(entry.value, depth)
		}
	} else {
		e.buf.WriteString("\n")
		e.indent++
		for _, entry := range entries {
			e.writeIndent()
			e.buf.Write(StringToBytes(entry.keyStr))
			e.writeSpace()
			e.buf.WriteString("=")
			e.writeSpace()
			e.encodeValue(entry.value, depth)
			e.buf.WriteString(",")
			e.buf.WriteString("\n")
		}
		e.indent--
		e.writeIndent()
	}
	e.buf.WriteString("]}")

	if cap(entries) <= maxPoolSliceCap {
		*entriesPtr = entries[:0]
		mapEntrySlicePool.Put(entriesPtr)
	}
}

func (e *internalEncoder) writeIndent() {
	if e.opts.Style != StyleSingleLine {
		n := e.indent
		for n > len(tabs) {
			e.buf.Write(tabs)
			n -= len(tabs)
		}
		e.buf.Write(tabs[:n])
	}
}
func (e *internalEncoder) writeNewLine() {
	if e.opts.Style != StyleSingleLine {
		e.buf.WriteString("\n")
	}
}
func (e *internalEncoder) writeSpace() {
	if e.opts.Style != StyleSingleLine {
		e.buf.WriteString(" ")
	}
}
func (e *internalEncoder) writeSeparator(isNotFirst, isCurrentBlockLike, isPrevBlockLike bool, depth int) {
	if !isNotFirst {
		return
	}
	if e.opts.Style == StyleSingleLine {
		e.buf.WriteString(";")
		return
	}
	e.writeNewLine()
	if depth == 0 && (e.opts.Style == StyleBlockSorted || e.opts.Style == StyleAllSorted) && e.opts.EmptyLines && (isCurrentBlockLike || isPrevBlockLike) {
		e.writeNewLine()
	}
}

func (e *streamInternalEncoder) writeQuotedString(s string) {
	if e.err != nil {
		return
	}
	i := 0
	for i < len(s) {
		b := s[i]
		if b < 0x20 || b == '\\' || b == '"' || b >= utf8.RuneSelf {
			break
		}
		i++
	}
	if i == len(s) {
		e.writeByte('"')
		e.writeString(s)
		e.writeByte('"')
		return
	}

	e.writeByte('"')
	e.writeString(s[:i])
	start := i
	for i < len(s) {
		if b := s[i]; b < utf8.RuneSelf {
			if 0x20 <= b && b != '\\' && b != '"' {
				i++
				continue
			}
			if start < i {
				e.writeString(s[start:i])
			}
			switch b {
			case '\\', '"':
				e.writeByte('\\')
				e.writeByte(b)
			case '\n':
				e.writeString("\\n")
			case '\r':
				e.writeString("\\r")
			case '\t':
				e.writeString("\\t")
			default:
				e.writeString(`\u00`)
				e.writeByte(hex[b>>4])
				e.writeByte(hex[b&0xF])
			}
			i++
			start = i
			continue
		}
		c, size := utf8.DecodeRuneInString(s[i:])
		if c == utf8.RuneError && size == 1 {
			if start < i {
				e.writeString(s[start:i])
			}
			e.writeString(`\ufffd`)
			i += size
			start = i
			continue
		}
		i += size
	}
	if start < len(s) {
		e.writeString(s[start:])
	}
	e.writeByte('"')
}

func (e *internalEncoder) writeQuotedString(s string) {
	i := 0
	for i < len(s) {
		b := s[i]
		if b < 0x20 || b == '\\' || b == '"' || b >= utf8.RuneSelf {
			break
		}
		i++
	}
	if i == len(s) {
		e.buf.WriteByte('"')
		e.buf.WriteString(s)
		e.buf.WriteByte('"')
		return
	}

	e.buf.WriteByte('"')
	e.buf.WriteString(s[:i])
	start := i
	for i < len(s) {
		if b := s[i]; b < utf8.RuneSelf {
			if 0x20 <= b && b != '\\' && b != '"' {
				i++
				continue
			}
			if start < i {
				e.buf.WriteString(s[start:i])
			}
			switch b {
			case '\\', '"':
				e.buf.WriteByte('\\')
				e.buf.WriteByte(b)
			case '\n':
				e.buf.WriteString("\\n")
			case '\r':
				e.buf.WriteString("\\r")
			case '\t':
				e.buf.WriteString("\\t")
			default:
				e.buf.WriteString(`\u00`)
				e.buf.WriteByte(hex[b>>4])
				e.buf.WriteByte(hex[b&0xF])
			}
			i++
			start = i
			continue
		}
		c, size := utf8.DecodeRuneInString(s[i:])
		if c == utf8.RuneError && size == 1 {
			if start < i {
				e.buf.WriteString(s[start:i])
			}
			e.buf.WriteString(`\ufffd`)
			i += size
			start = i
			continue
		}
		i += size
	}
	if start < len(s) {
		e.buf.WriteString(s[start:])
	}
	e.buf.WriteByte('"')
}

var hex = "0123456789abcdef"

func (e *streamInternalEncoder) encode(v any) error {
	rv := reflect.ValueOf(v)
	if rv.Kind() == reflect.Pointer {
		rv = rv.Elem()
	}
	if !rv.IsValid() {
		return fmt.Errorf("wanf: can only encode valid values")
	}
	if rv.Kind() == reflect.Struct {
		e.encodeStruct(rv, 0)
	} else {
		e.encodeValue(rv, 0)
	}
	if e.opts.Style != StyleSingleLine {
		e.writeString("\n")
	}
	return e.err
}

func (e *streamInternalEncoder) encodeStruct(v reflect.Value, depth int) {
	if e.err != nil {
		return
	}
	t := v.Type()
	cached, ok := fieldCache.Load(t)
	if !ok {
		cached = cacheStructInfo(t)
		fieldCache.Store(t, cached)
	}
	info := cached.(*cachedStructInfo)

	var cachedFields []cachedField
	if e.opts.NoSort {
		cachedFields = info.original
	} else {
		switch e.opts.Style {
		case StyleAllSorted:
			cachedFields = info.sorted
		case StyleBlockSorted:
			if depth > 0 {
				cachedFields = info.sorted
			} else {
				cachedFields = info.original
			}
		default:
			cachedFields = info.original
		}
	}

	var prevWasBlockLike bool
	var first = true
	for _, cf := range cachedFields {
		fv := v.Field(cf.index)
		val := fv
		if val.Kind() == reflect.Pointer {
			if val.IsNil() {
				continue
			}
			val = val.Elem()
		}

		if cf.tag.Omitempty {
			if (cf.isCollection && val.Len() == 0) || (!cf.isCollection && val.IsZero()) {
				continue
			}
		} else if cf.kind == reflect.Map && val.Len() == 0 {
			continue
		}

		e.writeSeparator(!first, cf.isBlockLike, prevWasBlockLike, depth)
		e.encodeField(cf, v, depth)
		prevWasBlockLike = cf.isBlockLike
		first = false
	}
}

func (e *streamInternalEncoder) encodeField(cf cachedField, v reflect.Value, depth int) {
	if e.err != nil {
		return
	}
	e.writeIndent()
	e.write(cf.nameBytes)
	e.writeSpace()

	fv := v.Field(cf.index)
	for fv.Kind() == reflect.Pointer {
		if fv.IsNil() {
			return
		}
		fv = fv.Elem()
	}
	if cf.isBlock {
		if fv.Kind() == reflect.Map {
			e.encodeMap(fv, depth+1)
		} else {
			e.writeString("{")
			e.writeNewLine()
			e.indent++
			e.encodeStruct(fv, depth+1)
			e.indent--
			e.writeNewLine()
			e.writeIndent()
			e.writeByte('}')
		}
	} else {
		e.writeString("=")
		e.writeSpace()
		e.encodeValue(fv, depth)
	}
}

func (e *streamInternalEncoder) encodeValue(v reflect.Value, depth int) {
	if e.err != nil || !v.IsValid() {
		return
	}

	if v.Kind() == reflect.Interface {
		if v.IsNil() {
			return
		}
		e.encodeInterface(v.Interface(), depth)
		return
	}

	for v.Kind() == reflect.Pointer {
		if v.IsNil() {
			return
		}
		v = v.Elem()
	}

	if v.Type() == durationType {
		e.writeString(time.Duration(v.Int()).String())
		return
	}

	switch v.Kind() {
	case reflect.String:
		s := v.String()
		if e.opts.Style != StyleSingleLine && strings.Contains(s, "\n") {
			e.writeByte('`')
			e.writeString(s)
			e.writeByte('`')
		} else {
			e.writeQuotedString(s)
		}
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		e.write(strconv.AppendInt(e.tmpBuf[:0], v.Int(), 10))
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		e.write(strconv.AppendUint(e.tmpBuf[:0], v.Uint(), 10))
	case reflect.Float32, reflect.Float64:
		e.write(strconv.AppendFloat(e.tmpBuf[:0], v.Float(), 'f', -1, 64))
	case reflect.Bool:
		e.write(strconv.AppendBool(e.tmpBuf[:0], v.Bool()))
	case reflect.Slice, reflect.Array:
		e.encodeSlice(v, depth)
	case reflect.Struct:
		if v.NumField() == 0 {
			e.writeString("{}")
			return
		}
		e.writeString("{")
		e.writeNewLine()
		e.indent++
		e.encodeStruct(v, depth+1)
		e.indent--
		e.writeNewLine()
		e.writeIndent()
		e.writeByte('}')
	case reflect.Map:
		if v.Type() == mapStringAnyType {
			e.encodeMapInterface(v.Interface().(map[string]any), depth)
			return
		}
		if v.Type() == mapStringStringType {
			e.encodeMapStringString(v.Interface().(map[string]string), depth)
			return
		}
		e.encodeMap(v, depth)
	}
}

func (e *streamInternalEncoder) encodeSlice(v reflect.Value, depth int) {
	if e.err != nil {
		return
	}
	e.writeString("[")
	l := v.Len()
	if l == 0 {
		e.writeByte(']')
		return
	}

	if e.opts.Style == StyleSingleLine {
		for i := 0; i < l; i++ {
			if i > 0 {
				e.writeString(",")
			}
			e.encodeValue(v.Index(i), depth)
		}
	} else {
		e.writeNewLine()
		e.indent++
		for i := 0; i < l; i++ {
			e.writeIndent()
			e.encodeValue(v.Index(i), depth)
			e.writeString(",")
			e.writeNewLine()
		}
		e.indent--
		e.writeIndent()
	}
	e.writeByte(']')
}

func (e *streamInternalEncoder) encodeInterface(i any, depth int) {
	if e.err != nil || i == nil {
		return
	}

	switch val := i.(type) {
	case string:
		if e.opts.Style != StyleSingleLine && strings.Contains(val, "\n") {
			e.writeByte('`')
			e.writeString(val)
			e.writeByte('`')
		} else {
			e.writeQuotedString(val)
		}
	case int:
		e.write(strconv.AppendInt(e.tmpBuf[:0], int64(val), 10))
	case int64:
		e.write(strconv.AppendInt(e.tmpBuf[:0], val, 10))
	case int32:
		e.write(strconv.AppendInt(e.tmpBuf[:0], int64(val), 10))
	case uint:
		e.write(strconv.AppendUint(e.tmpBuf[:0], uint64(val), 10))
	case uint64:
		e.write(strconv.AppendUint(e.tmpBuf[:0], val, 10))
	case uint32:
		e.write(strconv.AppendUint(e.tmpBuf[:0], uint64(val), 10))
	case float64:
		e.write(strconv.AppendFloat(e.tmpBuf[:0], val, 'f', -1, 64))
	case float32:
		e.write(strconv.AppendFloat(e.tmpBuf[:0], float64(val), 'f', -1, 32))
	case bool:
		e.write(strconv.AppendBool(e.tmpBuf[:0], val))
	case time.Duration:
		e.writeString(val.String())
	case map[string]any:
		e.encodeMapInterface(val, depth)
	case map[string]string:
		e.encodeMapStringString(val, depth)
	case []any:
		e.encodeSliceInterface(val, depth)
	case []string:
		e.encodeSliceString(val, depth)
	case int8:
		e.write(strconv.AppendInt(e.tmpBuf[:0], int64(val), 10))
	case int16:
		e.write(strconv.AppendInt(e.tmpBuf[:0], int64(val), 10))
	case uint8:
		e.write(strconv.AppendUint(e.tmpBuf[:0], uint64(val), 10))
	case uint16:
		e.write(strconv.AppendUint(e.tmpBuf[:0], uint64(val), 10))
	default:
		e.encodeValue(reflect.ValueOf(i), depth)
	}
}

func (e *streamInternalEncoder) encodeMapStringString(m map[string]string, depth int) {
	if e.err != nil {
		return
	}
	e.writeString("{[")
	if len(m) == 0 {
		e.writeString("]}")
		return
	}

	keysPtr := stringSlicePool.Get().(*[]string)
	keys := (*keysPtr)[:0]
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	if e.opts.Style == StyleSingleLine {
		for i, k := range keys {
			if i > 0 {
				e.writeString(",")
			}
			e.writeString(k)
			e.writeString("=")
			e.writeQuotedString(m[k])
		}
	} else {
		e.writeNewLine()
		e.indent++
		for _, k := range keys {
			e.writeIndent()
			e.writeString(k)
			e.writeSpace()
			e.writeString("=")
			e.writeSpace()
			e.writeQuotedString(m[k])
			e.writeString(",")
			e.writeNewLine()
		}
		e.indent--
		e.writeIndent()
	}
	e.writeString("]}")
	if cap(keys) <= maxPoolSliceCap {
		*keysPtr = keys
		stringSlicePool.Put(keysPtr)
	}
}

func (e *streamInternalEncoder) encodeSliceString(s []string, depth int) {
	if e.err != nil {
		return
	}
	e.writeString("[")
	l := len(s)
	if l == 0 {
		e.writeByte(']')
		return
	}

	if e.opts.Style == StyleSingleLine {
		for i, v := range s {
			if i > 0 {
				e.writeString(",")
			}
			e.writeQuotedString(v)
		}
	} else {
		e.writeNewLine()
		e.indent++
		for _, v := range s {
			e.writeIndent()
			e.writeQuotedString(v)
			e.writeString(",")
			e.writeNewLine()
		}
		e.indent--
		e.writeIndent()
	}
	e.writeByte(']')
}

func (e *streamInternalEncoder) encodeMapInterface(m map[string]any, depth int) {
	if e.err != nil {
		return
	}
	e.writeString("{[")
	if len(m) == 0 {
		e.writeString("]}")
		return
	}

	keysPtr := stringSlicePool.Get().(*[]string)
	keys := (*keysPtr)[:0]
	for k := range m {
		keys = append(keys, k)
	}
	sort.Strings(keys)

	if e.opts.Style == StyleSingleLine {
		for i, k := range keys {
			if i > 0 {
				e.writeString(",")
			}
			e.writeString(k)
			e.writeString("=")
			e.encodeInterface(m[k], depth)
		}
	} else {
		e.writeNewLine()
		e.indent++
		for _, k := range keys {
			e.writeIndent()
			e.writeString(k)
			e.writeSpace()
			e.writeString("=")
			e.writeSpace()
			e.encodeInterface(m[k], depth)
			e.writeString(",")
			e.writeNewLine()
		}
		e.indent--
		e.writeIndent()
	}
	e.writeString("]}")
	if cap(keys) <= maxPoolSliceCap {
		*keysPtr = keys
		stringSlicePool.Put(keysPtr)
	}
}

func (e *streamInternalEncoder) encodeSliceInterface(s []any, depth int) {
	if e.err != nil {
		return
	}
	e.writeString("[")
	l := len(s)
	if l == 0 {
		e.writeByte(']')
		return
	}

	if e.opts.Style == StyleSingleLine {
		for i, v := range s {
			if i > 0 {
				e.writeString(",")
			}
			e.encodeInterface(v, depth)
		}
	} else {
		e.writeNewLine()
		e.indent++
		for _, v := range s {
			e.writeIndent()
			e.encodeInterface(v, depth)
			e.writeString(",")
			e.writeNewLine()
		}
		e.indent--
		e.writeIndent()
	}
	e.writeByte(']')
}

func (e *streamInternalEncoder) encodeMap(v reflect.Value, depth int) {
	if e.err != nil {
		return
	}
	e.writeString("{[")
	l := v.Len()
	if l == 0 {
		e.writeString("]}")
		return
	}

	entriesPtr := mapEntrySlicePool.Get().(*[]mapEntry)
	entries := *entriesPtr
	if cap(entries) < l {
		entries = make([]mapEntry, 0, l)
	}
	entries = entries[:0]

	iter := v.MapRange()
	for iter.Next() {
		k := iter.Key()
		entries = append(entries, mapEntry{key: k, keyStr: k.String(), value: iter.Value()})
	}

	if len(entries) > 1 {
		quickSortMapEntries(entries)
	}

	if e.opts.Style == StyleSingleLine {
		for i, entry := range entries {
			if i > 0 {
				e.writeString(",")
			}
			e.writeString(entry.keyStr)
			e.writeString("=")
			e.encodeValue(entry.value, depth)
		}
	} else {
		e.writeNewLine()
		e.indent++
		for _, entry := range entries {
			e.writeIndent()
			e.writeString(entry.keyStr)
			e.writeSpace()
			e.writeString("=")
			e.writeSpace()
			e.encodeValue(entry.value, depth)
			e.writeString(",")
			e.writeNewLine()
		}
		e.indent--
		e.writeIndent()
	}
	e.writeString("]}")

	if cap(entries) <= maxPoolSliceCap {
		*entriesPtr = entries[:0]
		mapEntrySlicePool.Put(entriesPtr)
	}
}

func isBlockType(ft reflect.Type, tag wanfTag) bool {
	if ft.Kind() == reflect.Pointer {
		ft = ft.Elem()
	}
	// 只有结构体是块. 映射被视为值.
	// Only structs are blocks. Maps are treated as values.
	isStruct := ft.Kind() == reflect.Struct && ft.Name() != "Duration"
	return isStruct
}

func cacheStructInfo(t reflect.Type) *cachedStructInfo {
	var cachedFields []cachedField
	for i := 0; i < t.NumField(); i++ {
		fieldType := t.Field(i)
		if fieldType.PkgPath != "" {
			continue
		}
		tagStr := fieldType.Tag.Get("wanf")
		tagInfo := parseWanfTag(tagStr, fieldType.Name)
		ft := fieldType.Type
		if ft.Kind() == reflect.Pointer {
			ft = ft.Elem()
		}
		isBlock := isBlockType(ft, tagInfo)
		fk := ft.Kind()
		isBlockLike := isBlock || fk == reflect.Map || fk == reflect.Slice
		cachedFields = append(cachedFields, cachedField{
			name:         tagInfo.Name,
			nameBytes:    []byte(tagInfo.Name),
			tag:          tagInfo,
			fieldType:    fieldType,
			isBlock:      isBlock,
			isBlockLike:  isBlockLike,
			index:        i,
			kind:         fk,
			isCollection: fk == reflect.Map || fk == reflect.Slice,
		})
	}
	original := make([]cachedField, len(cachedFields))
	copy(original, cachedFields)

	// 预先排序缓存的字段, 减少运行时排序开销 (反射less)
	sort.Slice(cachedFields, func(i, j int) bool {
		if cachedFields[i].isBlock != cachedFields[j].isBlock {
			return !cachedFields[i].isBlock
		}
		return cachedFields[i].name < cachedFields[j].name
	})

	return &cachedStructInfo{
		original: original,
		sorted:   cachedFields,
	}
}

func isZero(v reflect.Value) bool {
	if !v.IsValid() {
		return true
	}
	return v.IsZero()
}

// --- Streaming Encoder ---

type StreamEncoder struct {
	w io.Writer
}

func NewStreamEncoder(w io.Writer, opts ...EncoderOption) *StreamEncoder {
	// For now, we just store the writer. The internal encoder will be set up in Encode.
	return &StreamEncoder{w: w}
}

func (enc *StreamEncoder) Encode(v any, opts ...EncoderOption) error {
	options := FormatOptions{
		Style:      StyleBlockSorted,
		EmptyLines: true,
	}
	for _, opt := range opts {
		opt(&options)
	}

	se := streamEncoderPool.Get().(*streamInternalEncoder)
	defer streamEncoderPool.Put(se)

	tmpBufPtr := byteSlicePool.Get().(*[]byte)
	se.tmpBuf = *tmpBufPtr
	defer func() {
		*tmpBufPtr = (*tmpBufPtr)[:0]
		byteSlicePool.Put(tmpBufPtr)
	}()

	// Reset the state of the pooled encoder
	bw := bufioWriterPool.Get().(*bufio.Writer)
	bw.Reset(enc.w)
	defer func() {
		bw.Reset(io.Discard)
		bufioWriterPool.Put(bw)
	}()

	se.w = bw
	se.indent = 0
	se.opts = options
	se.err = nil

	// Run the main encoding logic
	if err := se.encode(v); err != nil {
		return err
	}

	// Flush the buffer and return any I/O error
	return bw.Flush()
}

type streamInternalEncoder struct {
	w      *bufio.Writer
	indent int
	opts   FormatOptions
	err    error
	tmpBuf []byte
}

func (e *streamInternalEncoder) writeString(s string) {
	if e.err != nil {
		return
	}
	_, e.err = e.w.WriteString(s)
}

func (e *streamInternalEncoder) writeByte(b byte) {
	if e.err != nil {
		return
	}
	e.err = e.w.WriteByte(b)
}

func (e *streamInternalEncoder) write(p []byte) {
	if e.err != nil {
		return
	}
	_, e.err = e.w.Write(p)
}

// The following write helpers are adapted from the buffered encoder
// to work with the streaming encoder's error handling.
func (e *streamInternalEncoder) writeIndent() {
	if e.opts.Style != StyleSingleLine {
		n := e.indent
		for n > len(tabs) {
			e.write(tabs)
			n -= len(tabs)
		}
		e.write(tabs[:n])
	}
}
func (e *streamInternalEncoder) writeNewLine() {
	if e.opts.Style != StyleSingleLine {
		e.writeByte('\n')
	}
}
func (e *streamInternalEncoder) writeSpace() {
	if e.opts.Style != StyleSingleLine {
		e.writeString(" ")
	}
}
func (e *streamInternalEncoder) writeSeparator(isNotFirst, isCurrentBlockLike, isPrevBlockLike bool, depth int) {
	if !isNotFirst {
		return
	}
	if e.opts.Style == StyleSingleLine {
		e.writeString(";")
		return
	}
	e.writeNewLine()
	if depth == 0 && (e.opts.Style == StyleBlockSorted || e.opts.Style == StyleAllSorted) && e.opts.EmptyLines && (isCurrentBlockLike || isPrevBlockLike) {
		e.writeNewLine()
	}
}
