package wanf

import (
	"reflect"
	"strings"
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

func TestNeo_OmitEmpty_Bool(t *testing.T) {
	type Config struct {
		Active   bool `wanf:"active,omitempty"`
		Inactive bool `wanf:"inactive,omitempty"`
	}

	// Encode: both zero (false) → omit both
	cfg1 := Config{Active: false, Inactive: false}
	data, err := NeoMarshal(&cfg1)
	if err != nil {
		t.Fatalf("NeoMarshal failed: %v", err)
	}
	if string(data) != "" {
		t.Errorf("false+false: expected empty, got %q", string(data))
	}

	// Encode: one true, one false → only true field
	cfg2 := Config{Active: true, Inactive: false}
	data, err = NeoMarshal(&cfg2)
	if err != nil {
		t.Fatalf("NeoMarshal failed: %v", err)
	}
	if string(data) != "active = true" {
		t.Errorf("true+false: expected 'active = true', got %q", string(data))
	}

	// Encode: both true
	cfg3 := Config{Active: true, Inactive: true}
	data, err = NeoMarshal(&cfg3)
	if err != nil {
		t.Fatalf("NeoMarshal failed: %v", err)
	}
	if string(data) != "active = true\ninactive = true" {
		t.Errorf("true+true: unexpected output: %q", string(data))
	}

	// Round-trip: false values survive (explicit input)
	wanfData := []byte("active = false")
	var decoded Config
	err = NeoUnmarshal(wanfData, &decoded)
	if err != nil {
		t.Fatalf("NeoUnmarshal failed: %v", err)
	}
	if decoded.Active != false || decoded.Inactive != false {
		t.Errorf("Decode explicit false failed. Got active=%v, inactive=%v",
			decoded.Active, decoded.Inactive)
	}

	// Round-trip: omitempty fields absent → zero value
	var decoded2 Config
	err = NeoUnmarshal([]byte("active = true"), &decoded2)
	if err != nil {
		t.Fatalf("NeoUnmarshal failed: %v", err)
	}
	if decoded2.Active != true || decoded2.Inactive != false {
		t.Errorf("Decode expected active=true, inactive=false. Got active=%v, inactive=%v",
			decoded2.Active, decoded2.Inactive)
	}
}

func TestNeo_OmitEmpty_PointerBool(t *testing.T) {
	type Config struct {
		Flag *bool `wanf:"flag,omitempty"`
	}

	// Encode: nil pointer → omitted
	cfg1 := Config{Flag: nil}
	data, err := NeoMarshal(&cfg1)
	if err != nil {
		t.Fatalf("NeoMarshal failed: %v", err)
	}
	if string(data) != "" {
		t.Errorf("nil pointer: expected empty, got %q", string(data))
	}

	// Encode: &false → zero value, should be omitted
	f := false
	cfg2 := Config{Flag: &f}
	data, err = NeoMarshal(&cfg2)
	if err != nil {
		t.Fatalf("NeoMarshal failed: %v", err)
	}
	if string(data) != "" {
		t.Errorf("&false: expected empty (zero value), got %q", string(data))
	}

	// Encode: &true → present
	tVal := true
	cfg3 := Config{Flag: &tVal}
	data, err = NeoMarshal(&cfg3)
	if err != nil {
		t.Fatalf("NeoMarshal failed: %v", err)
	}
	if string(data) != "flag = true" {
		t.Errorf("&true: expected 'flag = true', got %q", string(data))
	}

	// Round-trip: explicit false ≠ absent
	wanfData := []byte("flag = false")
	var decoded Config
	err = NeoUnmarshal(wanfData, &decoded)
	if err != nil {
		t.Fatalf("NeoUnmarshal failed: %v", err)
	}
	if decoded.Flag == nil {
		t.Error("flag = false: expected non-nil pointer")
	} else if *decoded.Flag != false {
		t.Errorf("flag = false: expected false, got %v", *decoded.Flag)
	}

	// Round-trip: absent field → nil
	var decoded2 Config
	err = NeoUnmarshal([]byte("flag = true"), &decoded2)
	if err != nil {
		t.Fatalf("NeoUnmarshal failed: %v", err)
	}
	if decoded2.Flag == nil || *decoded2.Flag != true {
		t.Errorf("Expected flag=true, got %v", decoded2.Flag)
	}
}

func TestNeo_OmitEmpty_Decode_Compat(t *testing.T) {
	// Ensure encoding false+omitempty is indistinguishable from
	// the field being absent during later decode
	type Config struct {
		Active bool `wanf:"active,omitempty"`
	}

	// Encode false (omitted) → output empty
	cfg := Config{Active: false}
	data, _ := NeoMarshal(&cfg)
	if string(data) != "" {
		t.Fatalf("Expected empty, got %q", data)
	}

	// Decode the empty output → false (zero value)
	var decoded Config
	err := NeoUnmarshal(data, &decoded)
	if err != nil {
		t.Fatalf("NeoUnmarshal failed: %v", err)
	}
	if decoded.Active != false {
		t.Errorf("Decode of omitted bool: expected false, got %v", decoded.Active)
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

func TestNeo_DurationMicroseconds(t *testing.T) {
	type Config struct {
		Val time.Duration `wanf:"val"`
	}

	tests := []struct {
		literal string
		want    time.Duration
	}{
		{"10µs", 10 * time.Microsecond},
		{"10us", 10 * time.Microsecond},
		{"100µs", 100 * time.Microsecond},
		{"500ns", 500 * time.Nanosecond},
		{"1ms30µs", 1*time.Millisecond + 30*time.Microsecond},
	}

	for _, tt := range tests {
		t.Run("NeoDecode_"+tt.literal, func(t *testing.T) {
			wanfData := []byte("val = " + tt.literal)
			var cfg Config
			err := NeoUnmarshal(wanfData, &cfg)
			if err != nil {
				t.Fatalf("NeoUnmarshal(%q) failed: %v", tt.literal, err)
			}
			if cfg.Val != tt.want {
				t.Errorf("NeoUnmarshal(%q): got %v, want %v", tt.literal, cfg.Val, tt.want)
			}
		})

		t.Run("NeoRoundTrip_"+tt.literal, func(t *testing.T) {
			cfg := Config{Val: tt.want}
			data, err := NeoMarshal(&cfg)
			if err != nil {
				t.Fatalf("NeoMarshal failed: %v", err)
			}
			var decoded Config
			err = NeoUnmarshal(data, &decoded)
			if err != nil {
				t.Fatalf("NeoUnmarshal after marshal failed: %v", err)
			}
			if decoded.Val != cfg.Val {
				t.Errorf("Round-trip %q: got %v, want %v", tt.literal, decoded.Val, cfg.Val)
			}
		})
	}
}

func TestNeo_DurationMicroseconds_NeoLexer(t *testing.T) {
	tests := []struct {
		input           string
		expectedLiteral string
	}{
		{"10µs", "10µs"},
		{"10us", "10us"},
		{"1ms30µs", "1ms30µs"},
	}

	for _, tt := range tests {
		t.Run("NeoLexer_"+tt.input, func(t *testing.T) {
			l := NewNeoLexer(nil)
			l.SetInput([]byte(tt.input))
			tok := l.nextToken()
			l.Close()

			if tok.Type != DUR {
				t.Errorf("expected DUR type, got %v", tok.Type)
			}
			if string(tok.Literal) != tt.expectedLiteral {
				t.Errorf("literal: got %q, want %q", string(tok.Literal), tt.expectedLiteral)
			}
		})
	}
}

func TestNeo_Slice(t *testing.T) {
	type Config struct {
		Nums []int `wanf:"nums"`
	}

	cfg := Config{
		Nums: []int{1, 2, 3, 4, 5},
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

	if !reflect.DeepEqual(cfg, decoded) {
		t.Errorf("Slice round-trip failed.\nGot:  %+v\nWant: %+v", decoded, cfg)
	}
}

func TestNeo_Map(t *testing.T) {
	type Config struct {
		Data map[string]string `wanf:"data"`
	}

	cfg := Config{
		Data: map[string]string{
			"key1": "value1",
			"key2": "value2",
		},
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

	if !reflect.DeepEqual(cfg, decoded) {
		t.Errorf("Map round-trip failed.\nGot:  %+v\nWant: %+v", decoded, cfg)
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

// TestNeo_AnyField verifies Neo codec handles interface{}/any fields.
func TestNeo_AnyField(t *testing.T) {
	// Baseline: base Marshal works with any fields
	t.Run("Marshal_any_field_works", func(t *testing.T) {
		type Var struct {
			ID    string `wanf:"id"`
			Value any    `wanf:"value"`
		}
		v := Var{ID: "a", Value: float64(1.5)}
		data, err := Marshal(&v)
		if err != nil {
			t.Fatalf("Marshal failed: %v", err)
		}
		s := string(data)
		if !strings.Contains(s, "1.5") {
			t.Errorf("Marshal: expected output to contain '1.5', got %q", s)
		}
	})

	// NeoMarshal with any field writes value correctly
	t.Run("NeoMarshal_any_field_value", func(t *testing.T) {
		type Var struct {
			ID    string `wanf:"id"`
			Value any    `wanf:"value"`
		}
		v := Var{ID: "a", Value: float64(1.5)}
		data, err := NeoMarshal(&v)
		if err != nil {
			t.Fatalf("NeoMarshal failed: %v", err)
		}
		s := string(data)
		if !strings.Contains(s, "value = 1.5") {
			t.Errorf("NeoMarshal: expected 'value = 1.5', got %q", s)
		}
	})

	// NeoMarshal with concrete type works
	t.Run("NeoMarshal_concrete_field_works", func(t *testing.T) {
		type Var struct {
			ID    string  `wanf:"id"`
			Value float64 `wanf:"value"`
		}
		v := Var{ID: "a", Value: float64(1.5)}
		data, err := NeoMarshal(&v)
		if err != nil {
			t.Fatalf("NeoMarshal failed: %v", err)
		}
		s := string(data)
		if !strings.Contains(s, "1.5") {
			t.Errorf("NeoMarshal concrete: expected output to contain '1.5', got %q", s)
		}
	})

	// Round-trip: NeoMarshal + NeoUnmarshal with any
	t.Run("NeoRoundTrip_any_field", func(t *testing.T) {
		type Var struct {
			ID    string `wanf:"id"`
			Value any    `wanf:"value"`
		}
		v := Var{ID: "a", Value: float64(1.5)}
		data, err := NeoMarshal(&v)
		if err != nil {
			t.Fatalf("NeoMarshal failed: %v", err)
		}
		var decoded Var
		err = NeoUnmarshal(data, &decoded)
		if err != nil {
			t.Fatalf("NeoUnmarshal failed: %v", err)
		}
		if decoded.Value == nil {
			t.Fatal("NeoUnmarshal: any field value is nil after decode")
		}
		if decoded.Value.(float64) != 1.5 {
			t.Errorf("NeoUnmarshal: got %T(%v), want float64(1.5)", decoded.Value, decoded.Value)
		}
	})

	// Round-trip: Marshal + Decode with any → baseline
	t.Run("BaseRoundTrip_any_field_works", func(t *testing.T) {
		type Var struct {
			ID    string `wanf:"id"`
			Value any    `wanf:"value"`
		}
		v := Var{ID: "a", Value: float64(1.5)}
		data, err := Marshal(&v)
		if err != nil {
			t.Fatalf("Marshal failed: %v", err)
		}
		var decoded Var
		err = Decode(data, &decoded)
		if err != nil {
			t.Fatalf("Decode failed: %v", err)
		}
		if decoded.Value != float64(1.5) {
			t.Errorf("round-trip value: got %T(%v), want float64(1.5)", decoded.Value, decoded.Value)
		}
	})

	// omitempty with nil any field → skipped
	t.Run("NeoMarshal_omitempty_nil_any", func(t *testing.T) {
		type Var struct {
			Name string `wanf:"name,omitempty"`
			Val  any    `wanf:"val,omitempty"`
		}
		v := Var{Name: "test", Val: nil}
		data, err := NeoMarshal(&v)
		if err != nil {
			t.Fatalf("NeoMarshal failed: %v", err)
		}
		s := string(data)
		if strings.Contains(s, "val") {
			t.Errorf("omitempty nil any: expected 'val' to be omitted, got %q", s)
		}
		if !strings.Contains(s, "test") {
			t.Errorf("omitempty nil any: expected 'name' to be present, got %q", s)
		}
	})

	// omitempty with non-nil any field → written
	t.Run("NeoMarshal_omitempty_non_nil_any", func(t *testing.T) {
		type Var struct {
			Name string `wanf:"name,omitempty"`
			Val  any    `wanf:"val,omitempty"`
		}
		v := Var{Name: "test", Val: float64(2.0)}
		data, err := NeoMarshal(&v)
		if err != nil {
			t.Fatalf("NeoMarshal failed: %v", err)
		}
		s := string(data)
		if !strings.Contains(s, "val = 2") {
			t.Errorf("omitempty non-nil any: expected 'val = 2', got %q", s)
		}
	})
}

func TestNeo_VariableType(t *testing.T) {
	t.Run("handleVar_stores_int", func(t *testing.T) {
		wanfData := []byte(`
			var x = 123
			val = ${x}
		`)
		type Cfg struct {
			Val int `wanf:"val"`
		}
		var cfg Cfg
		err := NeoUnmarshal(wanfData, &cfg)
		if err != nil {
			t.Fatalf("NeoUnmarshal failed: %v", err)
		}
		if cfg.Val != 123 {
			t.Errorf("expected 123, got %v", cfg.Val)
		}
	})

	t.Run("handleVar_int_assigns_int64_field", func(t *testing.T) {
		wanfData := []byte(`
			var x = 123
			val = ${x}
		`)
		type Cfg struct {
			Val int64 `wanf:"val"`
		}
		var cfg Cfg
		err := NeoUnmarshal(wanfData, &cfg)
		if err != nil {
			t.Fatalf("NeoUnmarshal failed: %v", err)
		}
		if cfg.Val != int64(123) {
			t.Errorf("expected 123, got %v", cfg.Val)
		}
	})

	t.Run("decodeMapStringAny_int_value_type", func(t *testing.T) {
		// This test verifies the ACTUAL Go type stored in map[string]any
		// when decoding with NeoUnmarshal.
		wanfData := []byte(`
			data = {[
				value = 123,
			]}
		`)
		type Cfg struct {
			Data map[string]any `wanf:"data"`
		}
		var cfg Cfg
		err := NeoUnmarshal(wanfData, &cfg)
		if err != nil {
			t.Fatalf("NeoUnmarshal failed: %v", err)
		}
		val, ok := cfg.Data["value"]
		if !ok {
			t.Fatal("key 'value' not found in map")
		}
		t.Logf("decodeMapStringAny stores INT as: %T(%v)", val, val)

		if _, isInt64 := val.(int64); isInt64 {
			t.Log("Type is int64 (matches base decoder)")
		} else {
			t.Errorf("expected int64 type, got %T", val)
		}

		if val.(int64) != 123 {
			t.Errorf("value mismatch: got %v", val)
		}
	})

	t.Run("decodeMapStringAny_int_value_equality", func(t *testing.T) {
		// Following the same pattern as TestDecodeMapAny which uses
		// base decoder and expects int64(123). This tests the Neo path.
		wanfData := []byte(`
			data = {[
				value = 123,
			]}
		`)
		type Cfg struct {
			Data map[string]any `wanf:"data"`
		}
		var baseCfg Cfg
		err := Decode(wanfData, &baseCfg)
		if err != nil {
			t.Fatalf("Decode failed: %v", err)
		}
		t.Logf("Base decoder stores INT as: %T(%v)", baseCfg.Data["value"], baseCfg.Data["value"])

		var neoCfg Cfg
		err = NeoUnmarshal(wanfData, &neoCfg)
		if err != nil {
			t.Fatalf("NeoUnmarshal failed: %v", err)
		}
		t.Logf("Neo decoder stores INT as: %T(%v)", neoCfg.Data["value"], neoCfg.Data["value"])

		// Both should be int64 to match
		if _, ok := neoCfg.Data["value"].(int64); !ok {
			t.Errorf("Neo decoder: expected int64, got %T", neoCfg.Data["value"])
		}
	})
}
