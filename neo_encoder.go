package wanf

import (
	"bufio"
	"fmt"
	"io"
	"reflect"
	"strconv"
	"sync"
	"time"
	"unsafe"
)

type NeoEncoder struct {
	w      *bufio.Writer
	indent int
	err    error
	tmpBuf [64]byte // For numbers
}

var neoTabs = []byte("\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t\t")

var neoEncoderPool = sync.Pool{
	New: func() any {
		return &NeoEncoder{}
	},
}

func NewNeoEncoder(w io.Writer) *NeoEncoder {
	enc := neoEncoderPool.Get().(*NeoEncoder)
	bw := bufioWriterPool.Get().(*bufio.Writer)
	bw.Reset(w)
	enc.w = bw
	enc.indent = 0
	enc.err = nil
	return enc
}

func (enc *NeoEncoder) Close() {
	if enc.w != nil {
		enc.w.Flush()
		bufioWriterPool.Put(enc.w)
		enc.w = nil
	}
	neoEncoderPool.Put(enc)
}

func (enc *NeoEncoder) Encode(v any) error {
	rv := reflect.ValueOf(v)
	for rv.Kind() == reflect.Pointer {
		if rv.IsNil() {
			return nil
		}
		rv = rv.Elem()
	}

	if rv.Kind() != reflect.Struct {
		return nil
	}

	if !rv.CanAddr() {
		return fmt.Errorf("NeoEncoder: value must be addressable (pass a pointer)")
	}
	info := getNeoStructInfo(rv.Type())
	ptr := unsafe.Pointer(rv.UnsafeAddr())

	return enc.encodeStruct(info, ptr)
}

func (enc *NeoEncoder) encodeStruct(info *neoStructInfo, ptr unsafe.Pointer) error {
	first := true
	for _, f := range info.fields {
		fieldPtr := unsafe.Pointer(uintptr(ptr) + f.offset)

		// Check omitempty
		if f.tag.Omitempty && enc.isZero(f, fieldPtr) {
			continue
		}

		if !first {
			enc.writeNewLine()
		}
		first = false

		enc.writeIndent()
		enc.write(f.nameBytes)
		if f.isBlock {
			enc.writeString(" ")
		} else {
			enc.writeString(" = ")
		}

		enc.encodeField(f, fieldPtr)
	}
	return enc.err
}

func (enc *NeoEncoder) encodeField(f neoField, ptr unsafe.Pointer) {
	if f.isPtr {
		ptr = *(*unsafe.Pointer)(ptr)
		if ptr == nil {
			return
		}
	}
	switch f.kind {
	case reflect.String:
		enc.writeString("\"")
		enc.writeString(unsafeGetString(ptr))
		enc.writeString("\"")
	case reflect.Int:
		enc.write(strconv.AppendInt(enc.tmpBuf[:0], int64(unsafeGetInt(ptr)), 10))
	case reflect.Int64:
		if f.isDuration {
			enc.writeString(time.Duration(unsafeGetInt64(ptr)).String())
		} else {
			enc.write(strconv.AppendInt(enc.tmpBuf[:0], unsafeGetInt64(ptr), 10))
		}
	case reflect.Bool:
		if unsafeGetBool(ptr) {
			enc.writeString("true")
		} else {
			enc.writeString("false")
		}
	case reflect.Float64:
		enc.write(strconv.AppendFloat(enc.tmpBuf[:0], unsafeGetFloat64(ptr), 'f', -1, 64))
	case reflect.Struct:
		if f.structInfo != nil {
			enc.writeString("{")
			enc.writeNewLine()
			enc.indent++
			enc.encodeStruct(f.structInfo, ptr)
			enc.indent--
			enc.writeNewLine()
			enc.writeIndent()
			enc.writeString("}")
		}
	case reflect.Slice:
		enc.encodeSlice(f, ptr)
	case reflect.Map:
		enc.encodeMap(f, ptr)
	}
}

func (enc *NeoEncoder) isZero(f neoField, ptr unsafe.Pointer) bool {
	if f.isPtr {
		ptr = *(*unsafe.Pointer)(ptr)
		if ptr == nil {
			return true
		}
	}
	switch f.kind {
	case reflect.String:
		return unsafeGetString(ptr) == ""
	case reflect.Int:
		return unsafeGetInt(ptr) == 0
	case reflect.Int64:
		return unsafeGetInt64(ptr) == 0
	case reflect.Bool:
		return !unsafeGetBool(ptr)
	case reflect.Slice:
		rv := reflect.NewAt(f.elemType, ptr).Elem()
		return rv.Len() == 0
	case reflect.Map:
		rv := reflect.NewAt(f.elemType, ptr).Elem()
		return rv.IsNil() || rv.Len() == 0
	}
	return false
}

func (enc *NeoEncoder) write(p []byte) {
	if enc.err != nil {
		return
	}
	_, enc.err = enc.w.Write(p)
}

func (enc *NeoEncoder) writeString(s string) {
	if enc.err != nil {
		return
	}
	_, enc.err = enc.w.WriteString(s)
}

func (enc *NeoEncoder) writeIndent() {
	n := enc.indent
	for n > len(neoTabs) {
		enc.write(neoTabs)
		n -= len(neoTabs)
	}
	enc.write(neoTabs[:n])
}

func (enc *NeoEncoder) writeNewLine() {
	enc.writeString("\n")
}

func (enc *NeoEncoder) encodeSlice(f neoField, ptr unsafe.Pointer) {
	rv := reflect.NewAt(f.elemType, ptr).Elem()
	if rv.Len() == 0 {
		enc.writeString("[]")
		return
	}

	enc.writeString("[")
	elemType := f.elemType.Elem()

	fakeField := neoField{
		kind:     elemType.Kind(),
		elemType: elemType,
	}
	if elemType.Kind() == reflect.Struct {
		fakeField.structInfo = getNeoStructInfo(elemType)
	}

	for i := 0; i < rv.Len(); i++ {
		if i > 0 {
			enc.writeString(", ")
		}
		elem := rv.Index(i)
		enc.encodeReflectValue(fakeField, elem)
	}
	enc.writeString("]")
}

func (enc *NeoEncoder) encodeMap(f neoField, ptr unsafe.Pointer) {
	rv := reflect.NewAt(f.elemType, ptr).Elem()
	if rv.IsNil() || rv.Len() == 0 {
		enc.writeString("{[ ]}")
		return
	}

	enc.writeString("{[")
	enc.writeNewLine()
	enc.indent++

	valType := f.elemType.Elem()
	fakeField := neoField{
		kind:     valType.Kind(),
		elemType: valType,
	}
	if valType.Kind() == reflect.Struct {
		fakeField.structInfo = getNeoStructInfo(valType)
	}

	entriesPtr := mapEntrySlicePool.Get().(*[]mapEntry)
	entries := (*entriesPtr)[:0]

	iter := rv.MapRange()
	for iter.Next() {
		k := iter.Key()
		entries = append(entries, mapEntry{key: k, keyStr: k.String(), value: iter.Value()})
	}

	if len(entries) > 1 {
		quickSortMapEntries(entries)
	}

	for i, entry := range entries {
		if i > 0 {
			enc.writeNewLine()
		}
		enc.writeIndent()
		enc.writeString(entry.keyStr)
		enc.writeString(" = ")

		enc.encodeReflectValue(fakeField, entry.value)
		enc.writeString(",")
	}

	if cap(entries) <= maxPoolSliceCap {
		*entriesPtr = entries[:0]
		mapEntrySlicePool.Put(entriesPtr)
	}

	enc.indent--
	enc.writeNewLine()
	enc.writeIndent()
	enc.writeString("]}")
}

func (enc *NeoEncoder) encodeReflectValue(f neoField, v reflect.Value) {
	switch f.kind {
	case reflect.String:
		enc.writeString("\"")
		enc.writeString(v.String())
		enc.writeString("\"")
	case reflect.Int, reflect.Int64:
		if f.isDuration {
			enc.writeString(time.Duration(v.Int()).String())
		} else {
			enc.write(strconv.AppendInt(enc.tmpBuf[:0], v.Int(), 10))
		}
	case reflect.Bool:
		if v.Bool() {
			enc.writeString("true")
		} else {
			enc.writeString("false")
		}
	case reflect.Float64:
		enc.write(strconv.AppendFloat(enc.tmpBuf[:0], v.Float(), 'f', -1, 64))
	case reflect.Struct:
		if f.structInfo != nil {
			if v.CanAddr() {
				enc.writeString("{")
				enc.writeNewLine()
				enc.indent++
				enc.encodeStruct(f.structInfo, unsafe.Pointer(v.UnsafeAddr()))
				enc.indent--
				enc.writeNewLine()
				enc.writeIndent()
				enc.writeString("}")
			} else {
				v2 := reflect.New(v.Type()).Elem()
				v2.Set(v)
				enc.writeString("{")
				enc.writeNewLine()
				enc.indent++
				enc.encodeStruct(f.structInfo, unsafe.Pointer(v2.UnsafeAddr()))
				enc.indent--
				enc.writeNewLine()
				enc.writeIndent()
				enc.writeString("}")
			}
		}
	}
}
