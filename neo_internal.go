package wanf

import (
	"bytes"
	"reflect"
	"sync"
	"unsafe"
)

// neoField stores metadata for a struct field, including its memory offset.
type neoField struct {
	name         string
	nameBytes    []byte
	offset       uintptr
	kind         reflect.Kind
	isBlock      bool
	isCollection bool
	tag          wanfTag
	elemType     reflect.Type
	structInfo   *neoStructInfo
}

// neoStructInfo caches metadata for a struct type.
type neoStructInfo struct {
	fields []neoField
	byName map[string]*neoField // For decoding
}

var neoCache sync.Map // map[reflect.Type]*neoStructInfo

func getNeoStructInfo(t reflect.Type) *neoStructInfo {
	if info, ok := neoCache.Load(t); ok {
		return info.(*neoStructInfo)
	}

	if t.Kind() != reflect.Struct {
		return nil
	}

	info := &neoStructInfo{
		byName: make(map[string]*neoField),
	}

	for f := range t.Fields() {
		f := f
		if f.PkgPath != "" {
			continue
		}

		tagStr := f.Tag.Get("wanf")
		tagInfo := parseWanfTag(tagStr, f.Name)

		ft := f.Type
		if ft.Kind() == reflect.Pointer {
			ft = ft.Elem()
		}

		isBlock := isBlockType(ft, tagInfo)
		fk := ft.Kind()
		isCollection := fk == reflect.Map || fk == reflect.Slice

		nf := neoField{
			name:         tagInfo.Name,
			nameBytes:    []byte(tagInfo.Name),
			offset:       f.Offset,
			kind:         fk,
			isBlock:      isBlock,
			isCollection: isCollection,
			tag:          tagInfo,
			elemType:     f.Type,
		}

		if isBlock && fk == reflect.Struct {
			nf.structInfo = getNeoStructInfo(ft)
		}

		info.fields = append(info.fields, nf)
		info.byName[nf.name] = &info.fields[len(info.fields)-1]
	}

	neoCache.Store(t, info)
	return info
}

// Unsafe helpers
func unsafeSetString(ptr unsafe.Pointer, s string) {
	*(*string)(ptr) = s
}

func unsafeSetInt(ptr unsafe.Pointer, i int) {
	*(*int)(ptr) = i
}

func unsafeSetBool(ptr unsafe.Pointer, b bool) {
	*(*bool)(ptr) = b
}

func unsafeSetFloat64(ptr unsafe.Pointer, f float64) {
	*(*float64)(ptr) = f
}

func unsafeGetString(ptr unsafe.Pointer) string {
	return *(*string)(ptr)
}

func unsafeGetInt(ptr unsafe.Pointer) int {
	return *(*int)(ptr)
}

func unsafeGetBool(ptr unsafe.Pointer) bool {
	return *(*bool)(ptr)
}

func unsafeGetFloat64(ptr unsafe.Pointer) float64 {
	return *(*float64)(ptr)
}

func unsafeSetInt64(ptr unsafe.Pointer, i int64) {
	*(*int64)(ptr) = i
}

func unsafeSetUint64(ptr unsafe.Pointer, i uint64) {
	*(*uint64)(ptr) = i
}

func unsafeGetInt64(ptr unsafe.Pointer) int64 {
	return *(*int64)(ptr)
}

func unsafeGetUint64(ptr unsafe.Pointer) uint64 {
	return *(*uint64)(ptr)
}

func NeoMarshal(v any) ([]byte, error) {
	var buf bytes.Buffer
	enc := NewNeoEncoder(&buf)
	err := enc.Encode(v)
	enc.Close()
	return buf.Bytes(), err
}

func NeoUnmarshal(data []byte, v any) error {
	dec := NewNeoDecoder(bytes.NewReader(data))
	defer dec.Close()
	return dec.Decode(v)
}
