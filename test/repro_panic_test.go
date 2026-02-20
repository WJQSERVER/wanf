package wanf_test

import (
	"testing"
	"github.com/WJQSERVER/wanf"
)

type PointerConfig struct {
	SlicePtr *[]string `wanf:"slice_ptr,omitempty"`
	MapPtr   *map[string]string `wanf:"map_ptr,omitempty"`
}

func TestPointerPanic(t *testing.T) {
	// Case 1: Nil pointers
	c1 := PointerConfig{}
	_, err := wanf.Marshal(c1)
	if err != nil {
		t.Errorf("Nil pointers: Marshal failed: %v", err)
	}

	// Case 2: Pointer to empty slice/map
	s := []string{}
	m := map[string]string{}
	c2 := PointerConfig{
		SlicePtr: &s,
		MapPtr:   &m,
	}

	b, err := wanf.Marshal(c2)
	if err != nil {
		t.Errorf("Pointer to empty: Marshal failed: %v", err)
	}
	if len(b) > 0 && string(b) != "\n" {
		t.Errorf("Expected empty result for omitempty pointers, got %q", string(b))
	}
}

type LegacyMapPointer struct {
	MapPtr *map[string]string `wanf:"map_ptr"`
}

func TestLegacyMapPointer(t *testing.T) {
	m := map[string]string{}
	c := LegacyMapPointer{MapPtr: &m}
	b, err := wanf.Marshal(c)
	if err != nil {
		t.Fatal(err)
	}
	if len(b) > 0 && string(b) != "\n" {
		t.Errorf("Expected empty result for legacy map pointer, got %q", string(b))
	}
}
