package wanf

import (
	"bytes"
	"strings"
	"sync"
	"testing"
	"io"
)

type SyncMapStruct struct {
	Data sync.Map `wanf:"data"`
}

func TestSyncMapEncoding(t *testing.T) {
	var s SyncMapStruct
	s.Data.Store("key1", "value1")
	s.Data.Store("key2", 2)

	var buf bytes.Buffer
	encoder := NewEncoder(&buf)
	err := encoder.Encode(&s)
	if err != nil {
		t.Fatalf("Failed to encode: %v", err)
	}

	output := buf.String()
	t.Logf("Encoded output:\n%s", output)

	if !strings.Contains(output, "key1 = \"value1\"") {
		t.Errorf("Output missing key1")
	}
	if !strings.Contains(output, "key2 = 2") {
		t.Errorf("Output missing key2")
	}
}

func TestSyncMapDecodingAssignment(t *testing.T) {
	input := `
data = {
	key1 = "value1"
	key2 = 2
}
`
	var s SyncMapStruct
	err := Decode([]byte(input), &s)
	if err != nil {
		t.Fatalf("Failed to decode: %v", err)
	}

	v1, ok := s.Data.Load("key1")
	if !ok || v1 != "value1" {
		t.Errorf("Expected value1, got %v", v1)
	}

	v2, ok := s.Data.Load("key2")
	if !ok || v2 != int64(2) {
		t.Errorf("Expected 2, got %v (%T)", v2, v2)
	}
}

func TestSyncMapDecodingBlock(t *testing.T) {
	input := `
data "item1" {
	key1 = "value1"
}
`
	var s SyncMapStruct
	err := Decode([]byte(input), &s)
	if err != nil {
		t.Fatalf("Failed to decode: %v", err)
	}

	v, ok := s.Data.Load("item1")
	if !ok {
		t.Fatalf("item1 not found")
	}
	m, ok := v.(map[string]interface{})
	if !ok {
		t.Fatalf("Expected map[string]interface{}, got %T", v)
	}
	if m["key1"] != "value1" {
		t.Errorf("Expected value1, got %v", m["key1"])
	}
}

type PointerSyncMapStruct struct {
	Data *sync.Map `wanf:"data"`
}

func TestPointerSyncMapDecoding(t *testing.T) {
	input := `
data = {[
	key1 = "value1",
	key2 = 2
]}
`
	var s PointerSyncMapStruct
	err := Decode([]byte(input), &s)
	if err != nil {
		t.Fatalf("Failed to decode: %v", err)
	}

	if s.Data == nil {
		t.Fatalf("Expected s.Data to be initialized")
	}

	v1, _ := s.Data.Load("key1")
	if v1 != "value1" {
		t.Errorf("Expected value1, got %v", v1)
	}
}

func TestSyncMapOmitempty(t *testing.T) {
	type OmitemptyStruct struct {
		Data sync.Map `wanf:"data,omitempty"`
		Other string `wanf:"other"`
	}

	var s OmitemptyStruct
	s.Other = "test"

	var buf bytes.Buffer
	encoder := NewEncoder(&buf)
	err := encoder.Encode(&s)
	if err != nil {
		t.Fatalf("Failed to encode: %v", err)
	}

	output := buf.String()
	t.Logf("Encoded output:\n%s", output)

	if strings.Contains(output, "data") {
		t.Errorf("Output should not contain data when empty")
	}

	s.Data.Store("key", "val")
	buf.Reset()
	err = encoder.Encode(&s)
	if err != nil {
		t.Fatalf("Failed to encode: %v", err)
	}
	output = buf.String()
	t.Logf("Encoded output with data:\n%s", output)
	if !strings.Contains(output, "data") {
		t.Errorf("Output should contain data when not empty")
	}
}

func TestStreamSyncMapDecoding(t *testing.T) {
	input := `
data "item1" {
	key1 = "value1"
}
data "item2" {
    nested {
        a = 1
    }
}
`
	var s SyncMapStruct
	dec, err := NewStreamDecoder(strings.NewReader(input))
	if err != nil {
		t.Fatalf("Failed to create stream decoder: %v", err)
	}
	err = dec.Decode(&s)
	if err != nil && err != io.EOF {
		t.Fatalf("Failed to decode: %v", err)
	}

	v1, ok := s.Data.Load("item1")
	if !ok {
		t.Fatalf("item1 not found")
	}
	m1 := v1.(map[string]interface{})
	if m1["key1"] != "value1" {
		t.Errorf("Expected value1, got %v", m1["key1"])
	}

	v2, ok := s.Data.Load("item2")
	if !ok {
		t.Fatalf("item2 not found")
	}
	m2 := v2.(map[string]interface{})
	nested := m2["nested"].(map[string]interface{})
	if nested["a"] != int64(1) {
		t.Errorf("Expected 1, got %v", nested["a"])
	}
}

func TestStreamSyncMapEncoding(t *testing.T) {
	var s SyncMapStruct
	s.Data.Store("key1", "value1")

	var buf bytes.Buffer
	enc := NewStreamEncoder(&buf)
	err := enc.Encode(&s)
	if err != nil {
		t.Fatalf("Failed to encode: %v", err)
	}

	output := buf.String()
	t.Logf("Stream encoded output:\n%s", output)
	if !strings.Contains(output, "key1 = \"value1\"") {
		t.Errorf("Output missing key1")
	}
}
