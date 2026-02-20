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
	// Efficient lookup for []byte keys
	hashTable []int // Indices into fields slice, -1 means empty
	hashMask  int
}

func (info *neoStructInfo) findField(name []byte) *neoField {
	if len(info.hashTable) == 0 {
		return nil
	}
	h := hashBytes(name)
	idx := int(h) & info.hashMask
	for {
		fIdx := info.hashTable[idx]
		if fIdx == -1 {
			return nil
		}
		f := &info.fields[fIdx]
		if bytes.Equal(f.nameBytes, name) {
			return f
		}
		idx = (idx + 1) & info.hashMask
	}
}

func hashBytes(b []byte) uint32 {
	var h uint32 = 2166136261
	for _, c := range b {
		h *= 16777619
		h ^= uint32(c)
	}
	return h
}

var neoCache sync.Map // map[reflect.Type]*neoStructInfo

func getNeoStructInfo(t reflect.Type) *neoStructInfo {
	if info, ok := neoCache.Load(t); ok {
		return info.(*neoStructInfo)
	}

	if t.Kind() != reflect.Struct {
		return nil
	}

	info := &neoStructInfo{}

	for i := 0; i < t.NumField(); i++ {
		f := t.Field(i)
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
	}

	// Build hash table
	size := 1
	for size < len(info.fields)*2 {
		size *= 2
	}
	info.hashTable = make([]int, size)
	info.hashMask = size - 1
	for i := range info.hashTable {
		info.hashTable[i] = -1
	}
	for i, f := range info.fields {
		h := hashBytes(f.nameBytes)
		idx := int(h) & info.hashMask
		for info.hashTable[idx] != -1 {
			idx = (idx + 1) & info.hashMask
		}
		info.hashTable[idx] = i
	}

	neoCache.Store(t, info)
	return info
}

// Unsafe helpers (Ints, Bools, Strings) - kept as before
func unsafeSetString(ptr unsafe.Pointer, s string) { *(*string)(ptr) = s }
func unsafeSetInt(ptr unsafe.Pointer, i int) { *(*int)(ptr) = i }
func unsafeSetInt64(ptr unsafe.Pointer, i int64) { *(*int64)(ptr) = i }
func unsafeSetUint64(ptr unsafe.Pointer, i uint64) { *(*uint64)(ptr) = i }
func unsafeSetBool(ptr unsafe.Pointer, b bool) { *(*bool)(ptr) = b }
func unsafeSetFloat64(ptr unsafe.Pointer, f float64) { *(*float64)(ptr) = f }
func unsafeGetString(ptr unsafe.Pointer) string { return *(*string)(ptr) }
func unsafeGetInt(ptr unsafe.Pointer) int { return *(*int)(ptr) }
func unsafeGetInt64(ptr unsafe.Pointer) int64 { return *(*int64)(ptr) }
func unsafeGetUint64(ptr unsafe.Pointer) uint64 { return *(*uint64)(ptr) }
func unsafeGetBool(ptr unsafe.Pointer) bool { return *(*bool)(ptr) }
func unsafeGetFloat64(ptr unsafe.Pointer) float64 { return *(*float64)(ptr) }

func NeoMarshal(v any) ([]byte, error) {
	var buf bytes.Buffer
	enc := NewNeoEncoder(&buf)
	err := enc.Encode(v)
	enc.Close()
	return buf.Bytes(), err
}

func NeoUnmarshal(data []byte, v any) error {
	dec := NewNeoDecoderBytes(data)
	defer dec.Close()
	return dec.Decode(v)
}
