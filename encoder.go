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
		v := f.value
		if v.Kind() == reflect.Ptr {
			if v.IsNil() {
				return
			}
			v = v.Elem()
		}
		if v.Kind() == reflect.Map {
			e.encodeMap(v, depth+1)
		} else if v.Type() == syncMapType {
			e.encodeSyncMap(v, depth+1)
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
	if v.Kind() == reflect.Ptr || v.Kind() == reflect.Interface {
		if v.IsNil() {
			return
		}
		v = v.Elem()
	}
	if v.Type() == durationType {
		e.buf.WriteString(time.Duration(v.Int()).String())
		return
	}
	if v.Type() == syncMapType {
		e.encodeSyncMap(v, depth)
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

func (e *internalEncoder) encodeSyncMap(v reflect.Value, depth int) {
	var m *sync.Map
	if v.CanAddr() {
		m = v.Addr().Interface().(*sync.Map)
	} else {
		// If not addressable, we need to copy it to an addressable variable
		tmp := reflect.New(v.Type()).Elem()
		tmp.Set(v)
		m = tmp.Addr().Interface().(*sync.Map)
	}

	tmpMap := make(map[string]interface{})
	m.Range(func(k, v interface{}) bool {
		var key string
		if s, ok := k.(string); ok {
			key = s
		} else {
			key = fmt.Sprint(k)
		}
		tmpMap[key] = v
		return true
	})

	e.encodeMap(reflect.ValueOf(tmpMap), depth)
}

func (e *internalEncoder) encodeMap(v reflect.Value, depth int) {
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
		v := f.value
		if v.Kind() == reflect.Ptr {
			if v.IsNil() {
				return
			}
			v = v.Elem()
		}
		if v.Kind() == reflect.Map {
			e.encodeMap(v, depth+1)
		} else if v.Type() == syncMapType {
			e.encodeSyncMap(v, depth+1)
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
	if e.err != nil {
		return
	}
	if v.Kind() == reflect.Ptr || v.Kind() == reflect.Interface {
		if v.IsNil() {
			return
		}
		v = v.Elem()
	}
	if v.Type() == durationType {
		e.writeString(time.Duration(v.Int()).String())
		return
	}
	if v.Type() == syncMapType {
		e.encodeSyncMap(v, depth)
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

func (e *streamInternalEncoder) encodeSyncMap(v reflect.Value, depth int) {
	var m *sync.Map
	if v.CanAddr() {
		m = v.Addr().Interface().(*sync.Map)
	} else {
		tmp := reflect.New(v.Type()).Elem()
		tmp.Set(v)
		m = tmp.Addr().Interface().(*sync.Map)
	}

	tmpMap := make(map[string]interface{})
	m.Range(func(k, v interface{}) bool {
		var key string
		if s, ok := k.(string); ok {
			key = s
		} else {
			key = fmt.Sprint(k)
		}
		tmpMap[key] = v
		return true
	})

	e.encodeMap(reflect.ValueOf(tmpMap), depth)
}

func (e *streamInternalEncoder) encodeMap(v reflect.Value, depth int) {
	if e.err != nil {
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
	if v.Type() == syncMapType {
		var m *sync.Map
		if v.CanAddr() {
			m = v.Addr().Interface().(*sync.Map)
		} else {
			tmp := reflect.New(v.Type()).Elem()
			tmp.Set(v)
			m = tmp.Addr().Interface().(*sync.Map)
		}
		isEmpty := true
		m.Range(func(key, value interface{}) bool {
			isEmpty = false
			return false
		})
		return isEmpty
	}
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
