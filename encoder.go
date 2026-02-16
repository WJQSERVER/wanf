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

var durationType = reflect.TypeOf(time.Duration(0))

var encoderPool = sync.Pool{
	New: func() interface{} {
		return &internalEncoder{
			buf: &bytes.Buffer{},
		}
	},
}

// fieldCache caches processed field information for a given struct type.
var fieldCache sync.Map // map[reflect.Type][]cachedField

var fieldInfoSlicePool = sync.Pool{
	New: func() interface{} {
		s := make([]fieldInfo, 0, 16) // Start with a reasonable capacity
		return &s
	},
}

var byteSlicePool = sync.Pool{
	New: func() interface{} {
		b := make([]byte, 0, 64) // For strconv formatting
		return &b
	},
}

type mapEntry struct {
	key   reflect.Value
	value reflect.Value
}

var mapEntrySlicePool = sync.Pool{
	New: func() interface{} {
		s := make([]mapEntry, 0, 8) // Start with capacity for 8 map entries
		return &s
	},
}

var streamEncoderPool = sync.Pool{
	New: func() interface{} {
		return &streamInternalEncoder{}
	},
}

func getEncoder() *internalEncoder {
	return encoderPool.Get().(*internalEncoder)
}

func putEncoder(e *internalEncoder) {
	e.buf.Reset()
	e.indent = 0
	encoderPool.Put(e)
}

func Marshal(v interface{}) ([]byte, error) {
	var buf bytes.Buffer
	encoder := NewEncoder(&buf)
	if err := encoder.Encode(v); err != nil {
		return nil, err
	}
	return buf.Bytes(), nil
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

func (enc *Encoder) Encode(v interface{}) error {
	defer putEncoder(enc.e)

	tmpBufPtr := byteSlicePool.Get().(*[]byte)
	enc.e.tmpBuf = *tmpBufPtr
	defer func() {
		*tmpBufPtr = (*tmpBufPtr)[:0]
		byteSlicePool.Put(tmpBufPtr)
	}()

	rv := reflect.ValueOf(v)
	if rv.Kind() == reflect.Ptr {
		rv = rv.Elem()
	}
	if !rv.IsValid() || rv.Kind() != reflect.Struct {
		return fmt.Errorf("wanf: can only encode a non-nil struct")
	}
	if err := enc.e.encodeStruct(rv, 0); err != nil {
		return err
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

type fieldInfo struct {
	name        string
	value       reflect.Value
	tag         wanfTag
	fieldType   reflect.StructField
	isBlock     bool
	isBlockLike bool // for formatting
}

type cachedField struct {
	name        string
	tag         wanfTag
	fieldType   reflect.StructField
	isBlock     bool
	isBlockLike bool
	index       int
}

func (e *internalEncoder) encodeStruct(v reflect.Value, depth int) error {
	fieldsPtr := fieldInfoSlicePool.Get().(*[]fieldInfo)
	fields := *fieldsPtr
	gatherFields(v, &fields)

	if !e.opts.NoSort {
		switch e.opts.Style {
		case StyleBlockSorted, StyleAllSorted:
			if e.opts.Style == StyleAllSorted || depth > 0 {
				sort.Slice(fields, func(i, j int) bool {
					if fields[i].isBlock != fields[j].isBlock {
						return !fields[i].isBlock
					}
					return fields[i].name < fields[j].name
				})
			}
		case StyleStreaming:
		}
	}

	var prevWasBlockLike bool
	for i, f := range fields {
		e.writeSeparator(i > 0, f.isBlockLike, prevWasBlockLike, depth)
		e.encodeField(f, depth)
		prevWasBlockLike = f.isBlockLike
	}

	*fieldsPtr = fields[:0]
	fieldInfoSlicePool.Put(fieldsPtr)

	return nil
}

func (e *internalEncoder) encodeField(f fieldInfo, depth int) {
	e.writeIndent()
	e.buf.Write(StringToBytes(f.name))
	e.writeSpace()

	if f.isBlock {
		if f.value.Kind() == reflect.Map {
			e.encodeMap(f.value, depth+1)
		} else {
			e.buf.WriteString("{")
			e.writeNewLine()
			e.indent++
			e.encodeStruct(f.value, depth+1)
			e.indent--
			e.writeNewLine()
			e.writeIndent()
			e.buf.WriteString("}")
		}
	} else {
		e.buf.WriteString("=")
		e.writeSpace()
		e.encodeValue(f.value, depth)
	}
}

func (e *internalEncoder) encodeValue(v reflect.Value, depth int) {
	if !v.IsValid() {
		return
	}

	// 优先处理 interface{} 类型，使用 switch 枚举提高性能
	if v.Kind() == reflect.Interface && !v.IsNil() {
		e.encodeInterface(v.Interface(), depth)
		return
	}

	for v.Kind() == reflect.Ptr {
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

func (e *internalEncoder) encodeInterface(i interface{}, depth int) {
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
	case map[string]interface{}:
		e.encodeMapInterface(val, depth)
	case []interface{}:
		e.encodeSliceInterface(val, depth)
	default:
		// 回退到反射
		e.encodeValue(reflect.ValueOf(i), depth)
	}
}

func (e *internalEncoder) encodeMapInterface(m map[string]interface{}, depth int) {
	e.buf.WriteString("{[")
	if len(m) == 0 {
		e.buf.WriteString("]}")
		return
	}

	keys := make([]string, 0, len(m))
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
}

func (e *internalEncoder) encodeSliceInterface(s []interface{}, depth int) {
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

func (e *internalEncoder) encodeMap(v reflect.Value, depth int) {
	// 如果是 map[string]any，使用快速路径
	if v.Type().Key().Kind() == reflect.String && v.Type().Elem().Kind() == reflect.Interface {
		e.encodeMapInterface(v.Interface().(map[string]interface{}), depth)
		return
	}

	e.buf.WriteString("{[")
	if v.Len() == 0 {
		e.buf.WriteString("]}")
		return
	}

	entriesPtr := mapEntrySlicePool.Get().(*[]mapEntry)
	entries := *entriesPtr
	iter := v.MapRange()
	for iter.Next() {
		entries = append(entries, mapEntry{key: iter.Key(), value: iter.Value()})
	}

	sort.Slice(entries, func(i, j int) bool {
		return entries[i].key.String() < entries[j].key.String()
	})

	if e.opts.Style == StyleSingleLine {
		for i, entry := range entries {
			if i > 0 {
				e.buf.WriteString(",")
			}
			e.buf.Write(StringToBytes(entry.key.String()))
			e.buf.WriteString("=")
			e.encodeValue(entry.value, depth)
		}
	} else {
		e.buf.WriteString("\n")
		e.indent++
		for _, entry := range entries {
			e.writeIndent()
			e.buf.Write(StringToBytes(entry.key.String()))
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

	*entriesPtr = entries[:0]
	mapEntrySlicePool.Put(entriesPtr)
}

func (e *internalEncoder) writeIndent() {
	if e.opts.Style != StyleSingleLine {
		for i := 0; i < e.indent; i++ {
			e.buf.WriteByte('\t')
		}
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
	e.writeByte('"')
	start := 0
	for i := 0; i < len(s); {
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
	e.buf.WriteByte('"')
	start := 0
	for i := 0; i < len(s); {
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

func (e *streamInternalEncoder) encode(v interface{}) error {
	rv := reflect.ValueOf(v)
	if rv.Kind() == reflect.Ptr {
		rv = rv.Elem()
	}
	if !rv.IsValid() || rv.Kind() != reflect.Struct {
		return fmt.Errorf("wanf: can only encode a non-nil struct")
	}
	e.encodeStruct(rv, 0)
	if e.opts.Style != StyleSingleLine {
		e.writeString("\n")
	}
	return e.err
}

func (e *streamInternalEncoder) encodeStruct(v reflect.Value, depth int) {
	if e.err != nil {
		return
	}
	fieldsPtr := fieldInfoSlicePool.Get().(*[]fieldInfo)
	fields := *fieldsPtr
	gatherFields(v, &fields)

	if !e.opts.NoSort {
		switch e.opts.Style {
		case StyleBlockSorted, StyleAllSorted:
			if e.opts.Style == StyleAllSorted || depth > 0 {
				sort.Slice(fields, func(i, j int) bool {
					if fields[i].isBlock != fields[j].isBlock {
						return !fields[i].isBlock
					}
					return fields[i].name < fields[j].name
				})
			}
		case StyleStreaming:
		}
	}

	var prevWasBlockLike bool
	for i, f := range fields {
		e.writeSeparator(i > 0, f.isBlockLike, prevWasBlockLike, depth)
		e.encodeField(f, depth)
		prevWasBlockLike = f.isBlockLike
	}

	*fieldsPtr = fields[:0]
	fieldInfoSlicePool.Put(fieldsPtr)
}

func (e *streamInternalEncoder) encodeField(f fieldInfo, depth int) {
	if e.err != nil {
		return
	}
	e.writeIndent()
	e.writeString(f.name)
	e.writeSpace()

	if f.isBlock {
		if f.value.Kind() == reflect.Map {
			e.encodeMap(f.value, depth+1)
		} else {
			e.writeString("{")
			e.writeNewLine()
			e.indent++
			e.encodeStruct(f.value, depth+1)
			e.indent--
			e.writeNewLine()
			e.writeIndent()
			e.writeByte('}')
		}
	} else {
		e.writeString("=")
		e.writeSpace()
		e.encodeValue(f.value, depth)
	}
}

func (e *streamInternalEncoder) encodeValue(v reflect.Value, depth int) {
	if e.err != nil || !v.IsValid() {
		return
	}

	if v.Kind() == reflect.Interface && !v.IsNil() {
		e.encodeInterface(v.Interface(), depth)
		return
	}

	for v.Kind() == reflect.Ptr {
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

func (e *streamInternalEncoder) encodeInterface(i interface{}, depth int) {
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
	case map[string]interface{}:
		e.encodeMapInterface(val, depth)
	case []interface{}:
		e.encodeSliceInterface(val, depth)
	default:
		e.encodeValue(reflect.ValueOf(i), depth)
	}
}

func (e *streamInternalEncoder) encodeMapInterface(m map[string]interface{}, depth int) {
	if e.err != nil {
		return
	}
	e.writeString("{[")
	if len(m) == 0 {
		e.writeString("]}")
		return
	}

	keys := make([]string, 0, len(m))
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
}

func (e *streamInternalEncoder) encodeSliceInterface(s []interface{}, depth int) {
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
	// 如果是 map[string]any，使用快速路径
	if v.Type().Key().Kind() == reflect.String && v.Type().Elem().Kind() == reflect.Interface {
		e.encodeMapInterface(v.Interface().(map[string]interface{}), depth)
		return
	}

	e.writeString("{[")
	if v.Len() == 0 {
		e.writeString("]}")
		return
	}

	entriesPtr := mapEntrySlicePool.Get().(*[]mapEntry)
	entries := *entriesPtr
	iter := v.MapRange()
	for iter.Next() {
		entries = append(entries, mapEntry{key: iter.Key(), value: iter.Value()})
	}

	sort.Slice(entries, func(i, j int) bool {
		return entries[i].key.String() < entries[j].key.String()
	})

	if e.opts.Style == StyleSingleLine {
		for i, entry := range entries {
			if i > 0 {
				e.writeString(",")
			}
			e.writeString(entry.key.String())
			e.writeString("=")
			e.encodeValue(entry.value, depth)
		}
	} else {
		e.writeNewLine()
		e.indent++
		for _, entry := range entries {
			e.writeIndent()
			e.writeString(entry.key.String())
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

	*entriesPtr = entries[:0]
	mapEntrySlicePool.Put(entriesPtr)
}

func gatherFields(v reflect.Value, fields *[]fieldInfo) {
	if v.Kind() == reflect.Ptr {
		v = v.Elem()
	}
	if !v.IsValid() || v.Kind() != reflect.Struct {
		return
	}
	t := v.Type()
	cached, ok := fieldCache.Load(t)
	if !ok {
		cached = cacheStructInfo(t)
		fieldCache.Store(t, cached)
	}

	cachedFields := cached.([]cachedField)
	for _, cf := range cachedFields {
		fieldVal := v.Field(cf.index)
		if (cf.tag.Omitempty && isZero(fieldVal)) || (fieldVal.Kind() == reflect.Map && fieldVal.Len() == 0) {
			continue
		}
		*fields = append(*fields, fieldInfo{
			name:        cf.name,
			value:       fieldVal,
			tag:         cf.tag,
			fieldType:   cf.fieldType,
			isBlock:     cf.isBlock,
			isBlockLike: cf.isBlockLike,
		})
	}
}

func cacheStructInfo(t reflect.Type) []cachedField {
	var cachedFields []cachedField
	for i := 0; i < t.NumField(); i++ {
		fieldType := t.Field(i)
		if fieldType.PkgPath != "" {
			continue
		}
		tagStr := fieldType.Tag.Get("wanf")
		tagInfo := parseWanfTag(tagStr, fieldType.Name)
		ft := fieldType.Type
		if ft.Kind() == reflect.Ptr {
			ft = ft.Elem()
		}
		isBlock := isBlockType(ft, tagInfo)
		isBlockLike := isBlock || ft.Kind() == reflect.Map || ft.Kind() == reflect.Slice
		cachedFields = append(cachedFields, cachedField{
			name:        tagInfo.Name,
			tag:         tagInfo,
			fieldType:   fieldType,
			isBlock:     isBlock,
			isBlockLike: isBlockLike,
			index:       i,
		})
	}
	return cachedFields
}

func isBlockType(ft reflect.Type, tag wanfTag) bool {
	if ft.Kind() == reflect.Ptr {
		ft = ft.Elem()
	}
	// 只有结构体是块. 映射被视为值.
	// Only structs are blocks. Maps are treated as values.
	isStruct := ft.Kind() == reflect.Struct && ft.Name() != "Duration"
	return isStruct
}

func isZero(v reflect.Value) bool {
	switch v.Kind() {
	case reflect.String:
		return v.Len() == 0
	case reflect.Bool:
		return !v.Bool()
	case reflect.Int, reflect.Int8, reflect.Int16, reflect.Int32, reflect.Int64:
		return v.Int() == 0
	case reflect.Uint, reflect.Uint8, reflect.Uint16, reflect.Uint32, reflect.Uint64, reflect.Uintptr:
		return v.Uint() == 0
	case reflect.Float32, reflect.Float64:
		return v.Float() == 0
	case reflect.Interface, reflect.Ptr:
		return v.IsNil()
	case reflect.Slice, reflect.Map, reflect.Array:
		return v.Len() == 0
	}
	return false
}

// --- Streaming Encoder ---

type StreamEncoder struct {
	w io.Writer
}

func NewStreamEncoder(w io.Writer, opts ...EncoderOption) *StreamEncoder {
	// For now, we just store the writer. The internal encoder will be set up in Encode.
	return &StreamEncoder{w: w}
}

func (enc *StreamEncoder) Encode(v interface{}, opts ...EncoderOption) error {
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
	bw := bufio.NewWriter(enc.w)
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
	w      io.Writer
	indent int
	opts   FormatOptions
	err    error
	tmpBuf []byte
}

func (e *streamInternalEncoder) writeString(s string) {
	if e.err != nil {
		return
	}
	_, e.err = e.w.Write(StringToBytes(s))
}

func (e *streamInternalEncoder) writeByte(b byte) {
	if e.err != nil {
		return
	}
	// This is a common pattern for writing a single byte to an io.Writer
	// that doesn't have a WriteByte method.
	_, e.err = e.w.Write(singleCharByteSlices[b])
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
		for i := 0; i < e.indent; i++ {
			e.writeByte('\t')
		}
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
