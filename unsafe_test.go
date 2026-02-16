package wanf

import (
	"bytes"
	"testing"
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
