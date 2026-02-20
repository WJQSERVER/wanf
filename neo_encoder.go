package wanf

import (
	"fmt"
	"bufio"
	"io"
	"reflect"
	"strconv"
	"sync"
	"unsafe"
)

type NeoEncoder struct {
	w      *bufio.Writer
	indent int
	err    error
	tmpBuf [64]byte // For numbers
}

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
	enc.w.Flush()
	bufioWriterPool.Put(enc.w)
	enc.w = nil
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
		// Only supporting structs at top level for Neo for now
		return nil
	}

	if !rv.CanAddr() {
		// If not addressable, we might need to copy it to a new pointer or fail
		// For Neo, we expect pointers for maximum performance
		return fmt.Errorf("NeoEncoder: value must be addressable (pass a pointer)")
	}
	info := getNeoStructInfo(rv.Type())
	ptr := unsafe.Pointer(rv.UnsafeAddr())

	return enc.encodeStruct(info, ptr)
}

func (enc *NeoEncoder) encodeStruct(info *neoStructInfo, ptr unsafe.Pointer) error {
	for i, f := range info.fields {
		if i > 0 {
			enc.writeNewLine()
		}

		fieldPtr := unsafe.Pointer(uintptr(ptr) + f.offset)

		// Check omitempty
		if f.tag.Omitempty && enc.isZero(f, fieldPtr) {
			continue
		}

		enc.writeIndent()
		enc.write(f.nameBytes)
		enc.writeString(" = ")

		enc.encodeField(f, fieldPtr)
	}
	return enc.err
}

func (enc *NeoEncoder) encodeField(f neoField, ptr unsafe.Pointer) {
	switch f.kind {
	case reflect.String:
		enc.writeString("\"")
		enc.writeString(unsafeGetString(ptr))
		enc.writeString("\"")
	case reflect.Int:
		enc.write(strconv.AppendInt(enc.tmpBuf[:0], int64(unsafeGetInt(ptr)), 10))
	case reflect.Int64:
		enc.write(strconv.AppendInt(enc.tmpBuf[:0], unsafeGetInt64(ptr), 10))
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
	}
}

func (enc *NeoEncoder) isZero(f neoField, ptr unsafe.Pointer) bool {
	switch f.kind {
	case reflect.String:
		return unsafeGetString(ptr) == ""
	case reflect.Int:
		return unsafeGetInt(ptr) == 0
	case reflect.Int64:
		return unsafeGetInt64(ptr) == 0
	case reflect.Bool:
		return !unsafeGetBool(ptr)
	case reflect.Pointer:
		return *(*unsafe.Pointer)(ptr) == nil
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
	for i := 0; i < enc.indent; i++ {
		enc.writeString("\t")
	}
}

func (enc *NeoEncoder) writeNewLine() {
	enc.writeString("\n")
}

func (enc *NeoEncoder) encodeSlice(f neoField, ptr unsafe.Pointer) {
	header := (*reflect.SliceHeader)(ptr)
	if header.Len == 0 {
		enc.writeString("[]")
		return
	}

	enc.writeString("[")
	// This is tricky because we need to know the size of the element
	// and its encoding logic. For now, Neo is a proof-of-concept.
	// In a full implementation, we would pre-calculate element size.
	enc.writeString("...]") // Placeholder
}
