package wanf

import (
	"bytes"
	"reflect"
	"testing"
	"unsafe"
)

func TestUnsafeConversions(t *testing.T) {
	t.Run("StringToBytes", func(t *testing.T) {
		s := "hello world"
		b := StringToBytes(s)
		if !bytes.Equal(b, []byte(s)) {
			t.Errorf("expected %v, got %v", []byte(s), b)
		}
		if len(b) != len(s) {
			t.Errorf("expected length %d, got %d", len(s), len(b))
		}
	})

	t.Run("BytesToString", func(t *testing.T) {
		b := []byte("hello world")
		s := BytesToString(b)
		if s != "hello world" {
			t.Errorf("expected 'hello world', got '%s'", s)
		}
		if len(s) != len(b) {
			t.Errorf("expected length %d, got %d", len(b), len(s))
		}
	})

	t.Run("RoundTrip", func(t *testing.T) {
		s1 := "test string"
		b := StringToBytes(s1)
		s2 := BytesToString(b)
		if s1 != s2 {
			t.Errorf("expected %q, got %q", s1, s2)
		}
	})
}

func TestUnsafeHelpers(t *testing.T) {
	t.Run("Int", func(t *testing.T) {
		var i int
		ptr := unsafe.Pointer(&i)
		unsafeSetInt(ptr, 42)
		if i != 42 {
			t.Errorf("expected 42, got %d", i)
		}
		if unsafeGetInt(ptr) != 42 {
			t.Errorf("expected 42, got %d", unsafeGetInt(ptr))
		}
	})

	t.Run("String", func(t *testing.T) {
		var s string
		ptr := unsafe.Pointer(&s)
		unsafeSetString(ptr, "foo")
		if s != "foo" {
			t.Errorf("expected foo, got %s", s)
		}
		if unsafeGetString(ptr) != "foo" {
			t.Errorf("expected foo, got %s", unsafeGetString(ptr))
		}
	})

	t.Run("Bool", func(t *testing.T) {
		var b bool
		ptr := unsafe.Pointer(&b)
		unsafeSetBool(ptr, true)
		if !b {
			t.Error("expected true")
		}
		if !unsafeGetBool(ptr) {
			t.Error("expected true from getter")
		}
	})

	t.Run("Float64", func(t *testing.T) {
		var f float64
		ptr := unsafe.Pointer(&f)
		unsafeSetFloat64(ptr, 3.14)
		if f != 3.14 {
			t.Errorf("expected 3.14, got %f", f)
		}
		if unsafeGetFloat64(ptr) != 3.14 {
			t.Errorf("expected 3.14, got %f", unsafeGetFloat64(ptr))
		}
	})

	t.Run("Int64", func(t *testing.T) {
		var i int64
		ptr := unsafe.Pointer(&i)
		unsafeSetInt64(ptr, 1234567890)
		if i != 1234567890 {
			t.Errorf("expected 1234567890, got %d", i)
		}
		if unsafeGetInt64(ptr) != 1234567890 {
			t.Errorf("expected 1234567890, got %d", unsafeGetInt64(ptr))
		}
	})
}

func TestUnsafeFieldOffset(t *testing.T) {
	type Sample struct {
		A int
		B string
		C bool
	}

	s := Sample{A: 1, B: "init", C: false}
	typ := reflect.TypeOf(s)
	ptr := unsafe.Pointer(&s)

	fieldA := typ.Field(0)
	fieldB := typ.Field(1)
	fieldC := typ.Field(2)

	// Set values using offsets
	unsafeSetInt(unsafe.Pointer(uintptr(ptr)+fieldA.Offset), 100)
	unsafeSetString(unsafe.Pointer(uintptr(ptr)+fieldB.Offset), "updated")
	unsafeSetBool(unsafe.Pointer(uintptr(ptr)+fieldC.Offset), true)

	if s.A != 100 || s.B != "updated" || s.C != true {
		t.Errorf("Field updates via offset failed: %+v", s)
	}

	// Get values using offsets
	if unsafeGetInt(unsafe.Pointer(uintptr(ptr)+fieldA.Offset)) != 100 {
		t.Error("Failed to get A via offset")
	}
	if unsafeGetString(unsafe.Pointer(uintptr(ptr)+fieldB.Offset)) != "updated" {
		t.Error("Failed to get B via offset")
	}
	if !unsafeGetBool(unsafe.Pointer(uintptr(ptr)+fieldC.Offset)) {
		t.Error("Failed to get C via offset")
	}
}

func TestUnsafePointerToStruct(t *testing.T) {
	type Sub struct {
		X int
	}
	type Parent struct {
		Child *Sub
	}

	p := Parent{}
	ptr := unsafe.Pointer(&p)

	// Simulate what NeoDecoder does for f.isPtr
	fieldOffset := reflect.TypeOf(p).Field(0).Offset
	fieldPtr := unsafe.Pointer(uintptr(ptr) + fieldOffset)

	// Initialize the nil pointer
	if *(*unsafe.Pointer)(fieldPtr) == nil {
		newSub := reflect.New(reflect.TypeOf(Sub{}))
		*(*unsafe.Pointer)(fieldPtr) = unsafe.Pointer(newSub.Pointer())
	}

	if p.Child == nil {
		t.Fatal("Failed to initialize nil pointer via unsafe")
	}

	// Access field of the child
	childPtr := *(*unsafe.Pointer)(fieldPtr)
	childFieldOffset := reflect.TypeOf(Sub{}).Field(0).Offset
	unsafeSetInt(unsafe.Pointer(uintptr(childPtr)+childFieldOffset), 99)

	if p.Child.X != 99 {
		t.Errorf("Failed to set nested field via unsafe: got %d", p.Child.X)
	}
}
