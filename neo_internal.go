package wanf

import (
	"bytes"
	"reflect"
	"strings"
	"sync"
	"unsafe"
)

// neoField stores metadata for a struct field, including its memory offset.
type neoField struct {
	name         string
	nameBytes    []byte
	offset       uintptr
	kind         reflect.Kind
	isPtr        bool
	isBlock      bool
	isCollection bool
	isDuration   bool
	tag          wanfTag
	elemType     reflect.Type
	structInfo   *neoStructInfo
}

type neoHashEntry struct {
	keyB  []byte
	field *neoField
}

type neoHashTable struct {
	entries []neoHashEntry
	mask    uint32
}

func (t *neoHashTable) get(key []byte) *neoField {
	if len(t.entries) == 0 {
		return nil
	}
	h := hashBytes(key)
	idx := h & t.mask
	for {
		entry := &t.entries[idx]
		if entry.field == nil {
			return nil
		}
		if bytes.Equal(entry.keyB, key) {
			return entry.field
		}
		idx = (idx + 1) & t.mask
	}
}

func hashBytes(b []byte) uint32 {
	var h uint32 = 2166136261
	for _, c := range b {
		h ^= uint32(c)
		h *= 16777619
	}
	return h
}

// neoStructInfo caches metadata for a struct type.
type neoStructInfo struct {
	fields []neoField
	byName map[string]*neoField // For standard map lookup fallback
	table  neoHashTable         // For fast lookup
	tableL neoHashTable         // For fast case-insensitive lookup
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
		isPtr := ft.Kind() == reflect.Pointer
		if isPtr {
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
			isPtr:        isPtr,
			isBlock:      isBlock,
			isCollection: isCollection,
			isDuration:   ft == durationType,
			tag:          tagInfo,
			elemType:     f.Type,
		}

		if isBlock && fk == reflect.Struct {
			nf.structInfo = getNeoStructInfo(ft)
		}

		info.fields = append(info.fields, nf)
		ptr := &info.fields[len(info.fields)-1]
		info.byName[nf.name] = ptr
		lowerName := strings.ToLower(nf.name)
		if _, ok := info.byName[lowerName]; !ok {
			info.byName[lowerName] = ptr
		}
	}

	// Build hash tables
	size := 1
	for size < len(info.byName)*2 {
		size *= 2
	}
	info.table.entries = make([]neoHashEntry, size)
	info.table.mask = uint32(size - 1)
	info.tableL.entries = make([]neoHashEntry, size)
	info.tableL.mask = uint32(size - 1)

	for _, f := range info.fields {
		insertHashTable(&info.table, []byte(f.name), &f)
		insertHashTable(&info.tableL, []byte(strings.ToLower(f.name)), &f)
	}

	neoCache.Store(t, info)
	return info
}

func insertHashTable(t *neoHashTable, key []byte, f *neoField) {
	h := hashBytes(key)
	idx := h & t.mask
	for {
		entry := &t.entries[idx]
		if entry.field == nil {
			entry.keyB = key
			entry.field = f
			return
		}
		idx = (idx + 1) & t.mask
	}
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
	dec := NewNeoDecoderBytes(data)
	defer dec.Close()
	return dec.Decode(v)
}
