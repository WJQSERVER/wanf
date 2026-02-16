package wanf

import (
	"fmt"
	"io"
	"os"
	"path/filepath"
	"reflect"
	"strconv"
	"strings"
	"sync"
	"time"
)

var (
	decoderFieldCache sync.Map // map[reflect.Type]map[string]decoderCachedField
)

type decoderCachedField struct {
	Index    int
	Tag      wanfTag
	FieldTyp reflect.StructField
}

type DecoderOption func(*internalDecoder)

func WithBasePath(path string) DecoderOption {
	return func(d *internalDecoder) {
		d.basePath = path
	}
}

type Decoder struct {
	program *RootNode
	d       *internalDecoder
}

func NewDecoder(r io.Reader, opts ...DecoderOption) (*Decoder, error) {
	data, err := io.ReadAll(r)
	if err != nil {
		return nil, err
	}
	l := NewLexer(data)
	p := NewParser(l)
	program := p.ParseProgram()
	if len(p.Errors()) > 0 {
		var errs []string
		for _, err := range p.Errors() {
			errs = append(errs, err.Error())
		}
		return nil, fmt.Errorf("parser errors: %s", strings.Join(errs, "\n"))
	}
	d := &internalDecoder{vars: make(map[string]interface{})}
	for _, opt := range opts {
		opt(d)
	}
	finalStmts, err := processImports(program.Statements, d.basePath, make(map[string]bool))
	if err != nil {
		return nil, err
	}
	program.Statements = finalStmts
	for _, stmt := range program.Statements {
		if s, ok := stmt.(*VarStatement); ok {
			val, err := d.evalExpression(s.Value)
			if err != nil {
				return nil, err
			}
			d.vars[string(s.Name.Value)] = val
		}
	}
	return &Decoder{program: program, d: d}, nil
}

func processImports(stmts []Statement, basePath string, processed map[string]bool) ([]Statement, error) {
	var finalStmts []Statement
	for _, stmt := range stmts {
		importStmt, ok := stmt.(*ImportStatement)
		if !ok {
			finalStmts = append(finalStmts, stmt)
			continue
		}
		importPath := filepath.Join(basePath, string(importStmt.Path.Value))
		absImportPath, err := filepath.Abs(importPath)
		if err != nil {
			return nil, fmt.Errorf("could not get absolute path for import %q: %w", string(importStmt.Path.Value), err)
		}
		if processed[absImportPath] {
			continue
		}
		processed[absImportPath] = true
		data, err := os.ReadFile(absImportPath)
		if err != nil {
			return nil, fmt.Errorf("could not read imported file %q: %w", importPath, err)
		}
		l := NewLexer(data)
		p := NewParser(l)
		program := p.ParseProgram()
		if len(p.Errors()) > 0 {
			var errs []string
			for _, err := range p.Errors() {
				errs = append(errs, err.Error())
			}
			return nil, fmt.Errorf("parser errors in imported file %q: %s", importPath, strings.Join(errs, "\n"))
		}
		importedStmts, err := processImports(program.Statements, filepath.Dir(absImportPath), processed)
		if err != nil {
			return nil, err
		}
		finalStmts = append(finalStmts, importedStmts...)
	}
	return finalStmts, nil
}

func getOrCacheDecoderFields(typ reflect.Type) map[string]decoderCachedField {
	if cached, ok := decoderFieldCache.Load(typ); ok {
		return cached.(map[string]decoderCachedField)
	}

	fields := make(map[string]decoderCachedField)
	for i := 0; i < typ.NumField(); i++ {
		field := typ.Field(i)
		if field.PkgPath != "" {
			continue
		}

		tagStr := field.Tag.Get("wanf")
		tag := parseWanfTag(tagStr, field.Name)

		fields[tag.Name] = decoderCachedField{
			Index:    i,
			Tag:      tag,
			FieldTyp: field,
		}

		if tagStr == "" {
			if _, exists := fields[field.Name]; !exists {
				fields[field.Name] = decoderCachedField{
					Index:    i,
					Tag:      tag,
					FieldTyp: field,
				}
			}
		}
	}

	decoderFieldCache.Store(typ, fields)
	return fields
}

func (dec *Decoder) Decode(v interface{}) error {
	rv := reflect.ValueOf(v)
	if rv.Kind() != reflect.Ptr || rv.Elem().Kind() != reflect.Struct {
		return fmt.Errorf("v must be a pointer to a struct")
	}
	return dec.d.decodeRoot(dec.program, rv.Elem())
}

type internalDecoder struct {
	vars     map[string]interface{}
	basePath string
}

func (d *internalDecoder) decodeRoot(root *RootNode, rv reflect.Value) error {
	if rv.Kind() != reflect.Struct {
		return fmt.Errorf("can only decode root into a struct, got %s", rv.Kind())
	}
	for _, stmt := range root.Statements {
		switch s := stmt.(type) {
		case *AssignStatement:
			if err := d.decodeAssign(s, rv); err != nil {
				return err
			}
		case *BlockStatement:
			if err := d.decodeBlock(s, rv); err != nil {
				return err
			}
		}
	}
	return nil
}

func (d *internalDecoder) decodeAssign(stmt *AssignStatement, rv reflect.Value) error {
	field, tag, ok := findFieldAndTag(rv, stmt.Name.Value)
	if !ok {
		return nil
	}
	val, err := d.evalExpression(stmt.Value)
	if err != nil {
		return err
	}
	if tag.KeyField != "" {
		return d.setMapFromList(field, val, tag.KeyField)
	}
	return d.setField(field, val)
}

func (d *internalDecoder) decodeBlock(stmt *BlockStatement, rv reflect.Value) error {
	field, _, ok := findFieldAndTag(rv, stmt.Name.Value)
	if !ok {
		return nil
	}
	if field.Kind() == reflect.Ptr && field.Type().Elem().Kind() == reflect.Struct {
		if field.IsNil() {
			field.Set(reflect.New(field.Type().Elem()))
		}
		return d.decodeRoot(stmt.Body, field.Elem())
	}
	if field.Kind() == reflect.Struct {
		return d.decodeRoot(stmt.Body, field)
	}
	if field.Kind() == reflect.Map {
		mapType := field.Type()
		if mapType.Key().Kind() == reflect.String {
			if mapType.Elem().Kind() == reflect.String {
				if field.IsNil() {
					field.Set(reflect.MakeMap(mapType))
				}
				return d.decodeMapStringString(stmt.Body, field)
			}
			if mapType.Elem().Kind() == reflect.Interface {
				if field.IsNil() {
					field.Set(reflect.MakeMap(mapType))
				}
				if stmt.Label == nil {
					m, err := d.decodeBlockToMap(stmt.Body)
					if err != nil {
						return err
					}
					for k, v := range m {
						field.SetMapIndex(reflect.ValueOf(k), reflect.ValueOf(v))
					}
					return nil
				} else {
					m, err := d.decodeBlockToMap(stmt.Body)
					if err != nil {
						return err
					}
					field.SetMapIndex(reflect.ValueOf(string(stmt.Label.Value)), reflect.ValueOf(m))
					return nil
				}
			}
		}
		if stmt.Label == nil {
			return fmt.Errorf("block %q is for a map, but is missing a label", string(stmt.Name.Value))
		}
		mapVal := field
		if mapVal.IsNil() {
			mapVal.Set(reflect.MakeMap(mapVal.Type()))
		}
		elemType := mapVal.Type().Elem()
		newStruct := reflect.New(elemType).Elem()
		if err := d.decodeRoot(stmt.Body, newStruct); err != nil {
			return err
		}
		mapVal.SetMapIndex(reflect.ValueOf(string(stmt.Label.Value)), newStruct)
	}
	return nil
}

func (d *internalDecoder) setField(field reflect.Value, val interface{}) error {
	if field.Kind() == reflect.Interface {
		field.Set(reflect.ValueOf(val))
		return nil
	}
	if !field.CanSet() {
		return fmt.Errorf("cannot set field")
	}
	if field.Kind() == reflect.Ptr {
		if field.IsNil() {
			field.Set(reflect.New(field.Type().Elem()))
		}
		return d.setField(field.Elem(), val)
	}

	switch v := val.(type) {
	case string:
		switch field.Kind() {
		case reflect.String:
			field.SetString(v)
			return nil
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			if field.Type() == reflect.TypeOf(time.Duration(0)) {
				dur, err := time.ParseDuration(v)
				if err == nil {
					field.SetInt(int64(dur))
					return nil
				}
			}
			i, err := strconv.ParseInt(v, 0, field.Type().Bits())
			if err == nil {
				field.SetInt(i)
				return nil
			}
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
			i, err := strconv.ParseUint(v, 0, field.Type().Bits())
			if err == nil {
				field.SetUint(i)
				return nil
			}
		case reflect.Float32, reflect.Float64:
			f, err := strconv.ParseFloat(v, field.Type().Bits())
			if err == nil {
				field.SetFloat(f)
				return nil
			}
		case reflect.Bool:
			b, err := strconv.ParseBool(v)
			if err == nil {
				field.SetBool(b)
				return nil
			}
		}
	case int64:
		switch field.Kind() {
		case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
			field.SetInt(v)
			return nil
		case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64:
			field.SetUint(uint64(v))
			return nil
		case reflect.Float32, reflect.Float64:
			field.SetFloat(float64(v))
			return nil
		}
	case float64:
		if field.Kind() == reflect.Float32 || field.Kind() == reflect.Float64 {
			field.SetFloat(v)
			return nil
		}
	case bool:
		if field.Kind() == reflect.Bool {
			field.SetBool(v)
			return nil
		}
	}

	rv := reflect.ValueOf(val)
	if rv.Type().ConvertibleTo(field.Type()) {
		field.Set(rv.Convert(field.Type()))
		return nil
	}
	if field.Kind() == reflect.Map && rv.Kind() == reflect.Map {
		return d.setMapField(field, rv)
	}
	if field.Kind() == reflect.Slice && rv.Kind() == reflect.Slice {
		return d.setSliceField(field, rv)
	}
	return fmt.Errorf("cannot set field of type %s with value of type %T", field.Type(), val)
}

func (d *internalDecoder) setMapField(field, v reflect.Value) error {
	mapType := field.Type()
	if field.IsNil() {
		field.Set(reflect.MakeMap(mapType))
	}
	elemType := mapType.Elem()

	for _, key := range v.MapKeys() {
		val := v.MapIndex(key).Interface()
		valV := reflect.ValueOf(val)

		if elemType.Kind() == reflect.Struct {
			if elemType.NumField() == 0 {
				field.SetMapIndex(key, reflect.New(elemType).Elem())
				continue
			}

			sourceMap, ok := val.(map[string]interface{})
			if !ok {
				return fmt.Errorf("value for struct map must be a map object, got %T", val)
			}
			newStruct := reflect.New(elemType).Elem()
			if err := d.decodeMapToStruct(sourceMap, newStruct); err != nil {
				return err
			}
			field.SetMapIndex(key, newStruct)
			continue
		}

		if valV.Type().ConvertibleTo(elemType) {
			field.SetMapIndex(key, valV.Convert(elemType))
			continue
		}

		return fmt.Errorf("cannot convert map value %v to %s", val, elemType)
	}
	return nil
}

func (d *internalDecoder) setSliceField(field, v reflect.Value) error {
	sliceType := field.Type()
	elemType := sliceType.Elem()
	newSlice := reflect.MakeSlice(sliceType, v.Len(), v.Len())
	for i := 0; i < v.Len(); i++ {
		val := v.Index(i).Interface()

		if elemType.Kind() == reflect.Struct {
			if sourceMap, ok := val.(map[string]interface{}); ok {
				newStruct := reflect.New(elemType).Elem()
				if err := d.decodeMapToStruct(sourceMap, newStruct); err != nil {
					return err
				}
				newSlice.Index(i).Set(newStruct)
				continue
			}
		}

		valV := reflect.ValueOf(val)
		if valV.Type().ConvertibleTo(elemType) {
			newSlice.Index(i).Set(valV.Convert(elemType))
		} else {
			return fmt.Errorf("cannot convert slice element of type %T to %s", val, elemType)
		}
	}
	field.Set(newSlice)
	return nil
}

func (d *internalDecoder) evalExpression(expr Expression) (interface{}, error) {
	switch e := expr.(type) {
	case *IntegerLiteral:
		return e.Value, nil
	case *FloatLiteral:
		return e.Value, nil
	case *StringLiteral:
		return string(e.Value), nil
	case *BoolLiteral:
		return e.Value, nil
	case *DurationLiteral:
		return time.ParseDuration(string(e.Value))
	case *VarExpression:
		val, ok := d.vars[string(e.Name)]
		if !ok {
			return nil, fmt.Errorf("variable %q is not defined", string(e.Name))
		}
		return val, nil
	case *EnvExpression:
		val, found := os.LookupEnv(string(e.Name.Value))
		if !found {
			if e.DefaultValue != nil {
				return string(e.DefaultValue.Value), nil
			}
			return nil, fmt.Errorf("environment variable %q not set", string(e.Name.Value))
		}
		return val, nil
	case *ListLiteral:
		list := make([]interface{}, len(e.Elements))
		for i, elemExpr := range e.Elements {
			val, err := d.evalExpression(elemExpr)
			if err != nil {
				return nil, err
			}
			list[i] = val
		}
		return list, nil
	case *BlockLiteral:
		return d.decodeBlockToMap(e.Body)
	case *MapLiteral:
		return d.decodeMapLiteralToMap(e)
	}
	return nil, fmt.Errorf("unknown expression type: %T", expr)
}

func (d *internalDecoder) decodeMapLiteralToMap(ml *MapLiteral) (map[string]interface{}, error) {
	m := make(map[string]interface{}, len(ml.Elements))
	for _, stmt := range ml.Elements {
		assign, ok := stmt.(*AssignStatement)
		if !ok {
			return nil, fmt.Errorf("only 'key = value' assignments are allowed inside a map literal {[...]}, got %T", stmt)
		}
		val, err := d.evalExpression(assign.Value)
		if err != nil {
			return nil, err
		}
		m[string(assign.Name.Value)] = val
	}
	return m, nil
}

func (d *internalDecoder) decodeBlockToMap(body *RootNode) (map[string]interface{}, error) {
	m := make(map[string]interface{}, len(body.Statements))
	for _, stmt := range body.Statements {
		switch s := stmt.(type) {
		case *AssignStatement:
			val, err := d.evalExpression(s.Value)
			if err != nil {
				return nil, err
			}
			m[string(s.Name.Value)] = val
		case *BlockStatement:
			nestedMap, err := d.decodeBlockToMap(s.Body)
			if err != nil {
				return nil, err
			}
			m[string(s.Name.Value)] = nestedMap
		}
	}
	return m, nil
}

func findFieldAndTag(structVal reflect.Value, name []byte) (reflect.Value, wanfTag, bool) {
	typ := structVal.Type()
	cachedFields := getOrCacheDecoderFields(typ)

	sName := string(name)
	if f, ok := cachedFields[sName]; ok {
		return structVal.Field(f.Index), f.Tag, true
	}

	lowerName := strings.ToLower(sName)
	for _, f := range cachedFields {
		if f.Tag.Name == f.FieldTyp.Name && strings.ToLower(f.FieldTyp.Name) == lowerName {
			return structVal.Field(f.Index), f.Tag, true
		}
	}

	return reflect.Value{}, wanfTag{}, false
}

func (d *internalDecoder) setMapFromList(mapField reflect.Value, listVal interface{}, keyField string) error {
	if mapField.Kind() != reflect.Map {
		return fmt.Errorf("cannot set list to non-map field %s", mapField.Type())
	}
	sourceList, ok := listVal.([]interface{})
	if !ok {
		return fmt.Errorf("value for map field with 'key' tag must be a list")
	}
	if mapField.IsNil() {
		mapField.Set(reflect.MakeMap(mapField.Type()))
	}
	elemType := mapField.Type().Elem()
	for _, item := range sourceList {
		sourceMap, ok := item.(map[string]interface{})
		if !ok {
			return fmt.Errorf("items in list for keyed map must be objects")
		}
		keyVal, ok := sourceMap[keyField]
		if !ok {
			return fmt.Errorf("key field %q not found in list item", keyField)
		}
		keyString, ok := keyVal.(string)
		if !ok {
			return fmt.Errorf("key field %q must be a string", keyField)
		}
		newStruct := reflect.New(elemType).Elem()
		if err := d.decodeMapToStruct(sourceMap, newStruct); err != nil {
			return err
		}
		mapField.SetMapIndex(reflect.ValueOf(keyString), newStruct)
	}
	return nil
}

func (d *internalDecoder) decodeMapToStruct(sourceMap map[string]interface{}, targetStruct reflect.Value) error {
	for key, val := range sourceMap {
		field, _, ok := findFieldAndTag(targetStruct, []byte(key))
		if !ok {
			continue
		}
		if err := d.setField(field, val); err != nil {
			return fmt.Errorf("error setting field %q: %w", key, err)
		}
	}
	return nil
}

func (d *internalDecoder) decodeMapStringString(body *RootNode, mapField reflect.Value) error {
	for _, stmt := range body.Statements {
		assign, ok := stmt.(*AssignStatement)
		if !ok {
			continue
		}
		val, err := d.evalExpression(assign.Value)
		if err != nil {
			return err
		}
		strVal, ok := val.(string)
		if !ok {
			return fmt.Errorf("value for key %q in map must be a string", string(assign.Name.Value))
		}
		mapField.SetMapIndex(reflect.ValueOf(string(assign.Name.Value)), reflect.ValueOf(strVal))
	}
	return nil
}
