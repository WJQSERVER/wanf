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
	lcNameBytes  []byte
	offset       uintptr
	kind         reflect.Kind
	isBlock      bool
	isCollection bool
	isPointer    bool
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

	lcHashTable []int
	lcHashMask  int
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

func (info *neoStructInfo) findFieldCaseInsensitive(name []byte) *neoField {
	if len(info.lcHashTable) == 0 {
		return nil
	}
	var buf [64]byte
	var b []byte
	if len(name) <= 64 {
		b = buf[:len(name)]
	} else {
		b = make([]byte, len(name))
	}
	for i, c := range name {
		if c >= 'A' && c <= 'Z' {
			b[i] = c + 32
		} else {
			b[i] = c
		}
	}
	h := hashBytes(b)
	idx := int(h) & info.lcHashMask
	for {
		fIdx := info.lcHashTable[idx]
		if fIdx == -1 {
			return nil
		}
		f := &info.fields[fIdx]
		if bytes.Equal(f.lcNameBytes, b) {
			return f
		}
		idx = (idx + 1) & info.lcHashMask
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

		lcName := make([]byte, len(tagInfo.Name))
		for j, c := range tagInfo.Name {
			if c >= 'A' && c <= 'Z' {
				lcName[j] = byte(c + 32)
			} else {
				lcName[j] = byte(c)
			}
		}

		nf := neoField{
			name:         tagInfo.Name,
			nameBytes:    []byte(tagInfo.Name),
			lcNameBytes:  lcName,
			offset:       f.Offset,
			kind:         fk,
			isBlock:      isBlock,
			isCollection: isCollection,
			isPointer:    f.Type.Kind() == reflect.Pointer,
			tag:          tagInfo,
			elemType:     f.Type,
		}

		if isBlock && fk == reflect.Struct {
			nf.structInfo = getNeoStructInfo(ft)
		}

		info.fields = append(info.fields, nf)
	}

	// Build hash tables
	size := 1
	for size < len(info.fields)*2 {
		size *= 2
	}

	info.hashTable = make([]int, size)
	info.hashMask = size - 1
	for i := range info.hashTable { info.hashTable[i] = -1 }

	info.lcHashTable = make([]int, size)
	info.lcHashMask = size - 1
	for i := range info.lcHashTable { info.lcHashTable[i] = -1 }

	for i := range info.fields {
		f := &info.fields[i]
		// Exact
		h := hashBytes(f.nameBytes)
		idx := int(h) & info.hashMask
		for info.hashTable[idx] != -1 {
			idx = (idx + 1) & info.hashMask
		}
		info.hashTable[idx] = i

		// Case-insensitive
		h = hashBytes(f.lcNameBytes)
		idx = int(h) & info.lcHashMask
		for info.lcHashTable[idx] != -1 {
			// In case of conflict in lowercase names (unlikely in Go structs), we just take the first one
			if bytes.Equal(info.fields[info.lcHashTable[idx]].lcNameBytes, f.lcNameBytes) {
				break
			}
			idx = (idx + 1) & info.lcHashMask
		}
		if info.lcHashTable[idx] == -1 {
			info.lcHashTable[idx] = i
		}
	}

	neoCache.Store(t, info)
	return info
}

// Unsafe helpers (Ints, Bools, Strings)
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
