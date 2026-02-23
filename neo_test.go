package wanf

import (
	"reflect"
	"testing"
	"time"
)

func TestNeo_RoundTrip_Basic(t *testing.T) {
	type Config struct {
		Name   string  `wanf:"name"`
		Age    int     `wanf:"age"`
		Active bool    `wanf:"active"`
		Score  float64 `wanf:"score"`
	}

	cfg := Config{
		Name:   "Neo",
		Age:    30,
		Active: true,
		Score:  95.5,
	}

	data, err := NeoMarshal(&cfg)
	if err != nil {
		t.Fatalf("NeoMarshal failed: %v", err)
	}

	var decoded Config
	err = NeoUnmarshal(data, &decoded)
	if err != nil {
		t.Fatalf("NeoUnmarshal failed: %v\nData: %s", err, string(data))
	}

	if !reflect.DeepEqual(cfg, decoded) {
		t.Errorf("Round-trip failed.\nGot:  %+v\nWant: %+v", decoded, cfg)
	}
}

func TestNeo_NestedStruct(t *testing.T) {
	type Inner struct {
		Val int `wanf:"val"`
	}
	type Outer struct {
		Inner Inner `wanf:"inner"`
	}

	cfg := Outer{
		Inner: Inner{Val: 42},
	}

	data, err := NeoMarshal(&cfg)
	if err != nil {
		t.Fatalf("NeoMarshal failed: %v", err)
	}

	var decoded Outer
	err = NeoUnmarshal(data, &decoded)
	if err != nil {
		t.Fatalf("NeoUnmarshal failed: %v\nData: %s", err, string(data))
	}

	if !reflect.DeepEqual(cfg, decoded) {
		t.Errorf("Round-trip failed.\nGot:  %+v\nWant: %+v", decoded, cfg)
	}
}

func TestNeo_OmitEmpty(t *testing.T) {
	type Config struct {
		Name string `wanf:"name,omitempty"`
		Age  int    `wanf:"age,omitempty"`
	}

	cfg := Config{
		Name: "",
		Age:  0,
	}

	data, err := NeoMarshal(&cfg)
	if err != nil {
		t.Fatalf("NeoMarshal failed: %v", err)
	}

	s := string(data)
	if s != "" {
		t.Errorf("Expected empty output for omitempty fields, got: %q", s)
	}

	cfg.Name = "Neo"
	data, err = NeoMarshal(&cfg)
	s = string(data)
	if s != "name = \"Neo\"" {
		t.Errorf("Expected only name field, got: %q", s)
	}
}

func TestNeo_PointerFields(t *testing.T) {
	type Config struct {
		Name *string `wanf:"name"`
		Age  *int    `wanf:"age"`
	}

	name := "Neo"
	age := 30
	cfg := Config{
		Name: &name,
		Age:  &age,
	}

	data, err := NeoMarshal(&cfg)
	if err != nil {
		t.Fatalf("NeoMarshal failed: %v", err)
	}

	var decoded Config
	err = NeoUnmarshal(data, &decoded)
	if err != nil {
		t.Fatalf("NeoUnmarshal failed: %v", err)
	}

	if *decoded.Name != *cfg.Name || *decoded.Age != *cfg.Age {
		t.Errorf("Pointer fields round-trip failed. Got %+v, want %+v", decoded, cfg)
	}
}

func TestNeo_FieldMatching(t *testing.T) {
	type Config struct {
		TaggedField string `wanf:"tagged_field"`
		Name        string
	}

	wanfData := `
		tagged_field = "value1"
		name = "value2"
	`
	var cfg Config
	err := NeoUnmarshal([]byte(wanfData), &cfg)
	if err != nil {
		t.Fatalf("NeoUnmarshal failed: %v", err)
	}

	if cfg.TaggedField != "value1" || cfg.Name != "value2" {
		t.Errorf("Field matching failed. Got %+v", cfg)
	}

	// Test case-insensitivity
	wanfData2 := `
		TAGGED_FIELD = "value3"
		NAME = "value4"
	`
	var cfg2 Config
	err = NeoUnmarshal([]byte(wanfData2), &cfg2)
	if err != nil {
		t.Fatalf("NeoUnmarshal failed: %v", err)
	}

	if cfg2.TaggedField != "value3" || cfg2.Name != "value4" {
		t.Errorf("Case-insensitive field matching failed. Got %+v", cfg2)
	}
}

func TestNeo_Duration(t *testing.T) {
	type Config struct {
		Timeout time.Duration `wanf:"timeout"`
	}

	tests := []struct {
		name string
		val  time.Duration
	}{
		{"Positive", 10 * time.Second},
		{"Negative", -10 * time.Second},
		{"Zero", 0},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			cfg := Config{Timeout: tt.val}
			data, err := NeoMarshal(&cfg)
			if err != nil {
				t.Fatalf("NeoMarshal failed: %v", err)
			}
			t.Logf("Encoded duration (%s): %s", tt.name, string(data))

			var decoded Config
			err = NeoUnmarshal(data, &decoded)
			if err != nil {
				t.Fatalf("NeoUnmarshal failed: %v", err)
			}

			if decoded.Timeout != cfg.Timeout {
				t.Errorf("Round-trip failed. Got %v, want %v", decoded.Timeout, cfg.Timeout)
			}
		})
	}
}

func TestNeo_DurationLiteral(t *testing.T) {
	type Config struct {
		Timeout time.Duration `wanf:"timeout"`
	}

	wanfData := `timeout = -10s`
	var cfg Config
	err := NeoUnmarshal([]byte(wanfData), &cfg)
	if err != nil {
		t.Fatalf("NeoUnmarshal failed for literal: %v", err)
	}

	if cfg.Timeout != -10*time.Second {
		t.Errorf("Expected -10s, got %v", cfg.Timeout)
	}
}
