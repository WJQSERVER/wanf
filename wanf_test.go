package wanf

import (
	"bytes"
	"reflect"
	"strings"
	"testing"
)

func TestEncoder_Styles(t *testing.T) {
	type SubBlock struct {
		B_sub_kv  string            `wanf:"b_sub_kv"`
		A_sub_kv  string            `wanf:"a_sub_kv"`
		C_sub_map map[string]string `wanf:"c_sub_map"`
	}
	type Config struct {
		C_kv    string   `wanf:"c_kv"`
		A_block SubBlock `wanf:"a_block"`
		B_kv    int      `wanf:"b_kv"`
	}

	testData := Config{
		C_kv: "c",
		A_block: SubBlock{
			B_sub_kv: "b",
			A_sub_kv: "a",
			C_sub_map: map[string]string{
				"sub_key": "sub_val",
			},
		},
		B_kv: 123,
	}

	tests := []struct {
		name    string
		options []EncoderOption
		want    string
	}{
		{
			"Default (StyleBlockSorted)",
			[]EncoderOption{},
			`c_kv = "c"

a_block {
	a_sub_kv = "a"
	b_sub_kv = "b"
	c_sub_map = {[
		sub_key = "sub_val",
	]}
}

b_kv = 123`,
		},
		{
			"StyleAllSorted",
			[]EncoderOption{WithStyle(StyleAllSorted)},
			`b_kv = 123
c_kv = "c"

a_block {
	a_sub_kv = "a"
	b_sub_kv = "b"
	c_sub_map = {[
		sub_key = "sub_val",
	]}
}`,
		},
		{
			"StyleStreaming",
			[]EncoderOption{WithStyle(StyleStreaming)},
			`c_kv = "c"
a_block {
	b_sub_kv = "b"
	a_sub_kv = "a"
	c_sub_map = {[
		sub_key = "sub_val",
	]}
}
b_kv = 123`,
		},
		{
			"StyleSingleLine",
			[]EncoderOption{WithStyle(StyleSingleLine)},
			`c_kv="c";a_block{b_sub_kv="b";a_sub_kv="a";c_sub_map={[sub_key="sub_val"]}};b_kv=123`,
		},
	}

	for _, tt := range tests {
		t.Run(tt.name, func(t *testing.T) {
			var buf bytes.Buffer
			encoder := NewEncoder(&buf, tt.options...)
			if err := encoder.Encode(testData); err != nil {
				t.Fatalf("Encode() with style %v failed: %v", tt.name, err)
			}
			got := strings.TrimSuffix(buf.String(), "\n")
			if got != tt.want {
				t.Errorf("Encode() with style %v got:\n%q\nWant:\n%q", tt.name, got, tt.want)
			}

			var decodedCfg Config
			if err := Decode(buf.Bytes(), &decodedCfg); err != nil {
				t.Fatalf("Decode round-trip failed for style %v: %v\nGot:\n%s", tt.name, err, buf.String())
			}
			if !reflect.DeepEqual(testData, decodedCfg) {
				t.Errorf("Round-trip failed for style %v. Got %+v, want %+v", tt.name, decodedCfg, testData)
			}
		})
	}
}

func TestEncoder_EmptyMap(t *testing.T) {
	type ConfigWithMap struct {
		Name  string            `wanf:"name"`
		Attrs map[string]string `wanf:"attrs"`
	}
	cfg := ConfigWithMap{Name: "test", Attrs: make(map[string]string)}
	b, err := Marshal(cfg)
	if err != nil {
		t.Fatalf("Failed to marshal with empty map: %v", err)
	}
	if strings.Contains(string(b), "attrs") {
		t.Errorf("Encoder should omit empty map field. Got: %s", string(b))
	}
}

func TestDecode_CompactFormat(t *testing.T) {
	compactData := `enabled=true;server{port=8080};ids=[1,2,3]`
	var cfg struct {
		Enabled bool `wanf:"enabled"`
		Server  struct {
			Port int `wanf:"port"`
		} `wanf:"server"`
		IDs []any `wanf:"ids"`
	}
	err := Decode([]byte(compactData), &cfg)
	if err != nil {
		t.Fatalf("Decode failed for compact format: %v", err)
	}
	if !(cfg.Enabled && cfg.Server.Port == 8080 && len(cfg.IDs) == 3) {
		t.Errorf("Compact format decode mismatch. Got: %+v", cfg)
	}
}

func TestFieldMatching_Fallback(t *testing.T) {
	type Config struct {
		TaggedField   string `wanf:"tagged_field"`
		UntaggedField int
		LogLevel      string // Test case-insensitivity
	}

	// Test encoding
	cfg := Config{
		TaggedField:   "value1",
		UntaggedField: 123,
		LogLevel:      "INFO",
	}
	b, err := Marshal(cfg)
	if err != nil {
		t.Fatalf("Marshal failed: %v", err)
	}
	encoded := string(b)
	if !strings.Contains(encoded, "tagged_field = \"value1\"") {
		t.Errorf("Expected tagged field to be encoded")
	}
	if !strings.Contains(encoded, "UntaggedField = 123") {
		t.Errorf("Expected untagged field to be encoded with its field name")
	}
	if !strings.Contains(encoded, "LogLevel = \"INFO\"") {
		t.Errorf("Expected untagged field to be encoded with its field name")
	}

	// Test decoding
	wanfData := `
		tagged_field = "new_value"
		untaggedfield = 456
		loglevel = "DEBUG"
	`
	var decodedCfg Config
	err = Decode([]byte(wanfData), &decodedCfg)
	if err != nil {
		t.Fatalf("Decode failed: %v", err)
	}

	expected := Config{
		TaggedField:   "new_value",
		UntaggedField: 456,
		LogLevel:      "DEBUG",
	}
	if !reflect.DeepEqual(decodedCfg, expected) {
		t.Errorf("Fallback decoding failed. Got %+v, want %+v", decodedCfg, expected)
	}
}

func TestMapAndListStyles(t *testing.T) {
	type Nested struct {
		Val int `wanf:"val"`
	}
	type Config struct {
		StrMap   map[string]string   `wanf:"str_map"`
		Set      map[string]struct{} `wanf:"set"`
		ObjMap   map[string]Nested   `wanf:"obj_map"`
		StrList  []string            `wanf:"str_list"`
		EmptyMap map[string]string   `wanf:"empty_map"`
	}

	cfg := Config{
		StrMap: map[string]string{"b": "B", "a": "A"},
		Set: map[string]struct{}{
			"z": {},
			"y": {},
		},
		ObjMap: map[string]Nested{
			"n2": {Val: 2},
			"n1": {Val: 1},
		},
		StrList:  []string{"c", "d"},
		EmptyMap: make(map[string]string),
	}

	// Note: for StyleSingleLine, blocks inside maps are not sorted due to implementation simplicity.
	// This is acceptable as single line is for machines, not humans.
	expectedOutputs := map[string]string{
		"StyleBlockSorted": `str_map = {[
	a = "A",
	b = "B",
]}

set = {[
	y = {},
	z = {},
]}

obj_map = {[
	n1 = {
		val = 1
	},
	n2 = {
		val = 2
	},
]}

str_list = [
	"c",
	"d",
]`,
		"StyleSingleLine": `str_map={[a="A",b="B"]};set={[y={},z={}]};obj_map={[n1={val=1},n2={val=2}]};str_list=["c","d"]`,
	}

	// Test Block Sorted (Default)
	t.Run("StyleBlockSorted", func(t *testing.T) {
		var buf bytes.Buffer
		enc := NewEncoder(&buf, WithStyle(StyleBlockSorted))
		if err := enc.Encode(cfg); err != nil {
			t.Fatalf("Encode failed: %v", err)
		}
		// Trim trailing newline for comparison
		got := strings.TrimSuffix(buf.String(), "\n")
		want := expectedOutputs["StyleBlockSorted"]
		if got != want {
			t.Errorf("got:\n%s\n\nwant:\n%s", got, want)
		}

		// Round-trip
		var decodedCfg Config
		if err := Decode(buf.Bytes(), &decodedCfg); err != nil {
			t.Fatalf("Decode round-trip failed: %v", err)
		}
		if !reflect.DeepEqual(cfg.Set, decodedCfg.Set) || !reflect.DeepEqual(cfg.StrMap, decodedCfg.StrMap) {
			t.Errorf("Round-trip failed. Got %+v, want %+v", decodedCfg, cfg)
		}
	})

	// Test Single Line
	t.Run("StyleSingleLine", func(t *testing.T) {
		var buf bytes.Buffer
		enc := NewEncoder(&buf, WithStyle(StyleSingleLine))
		if err := enc.Encode(cfg); err != nil {
			t.Fatalf("Encode failed: %v", err)
		}
		got := buf.String()
		want := expectedOutputs["StyleSingleLine"]
		if got != want {
			t.Errorf("got:\n%s\n\nwant:\n%s", got, want)
		}
		// Round-trip
		var decodedCfg Config
		if err := Decode(buf.Bytes(), &decodedCfg); err != nil {
			t.Fatalf("Decode round-trip failed: %v", err)
		}
		if !reflect.DeepEqual(cfg.Set, decodedCfg.Set) || !reflect.DeepEqual(cfg.StrMap, decodedCfg.StrMap) {
			t.Errorf("Round-trip failed. Got %+v, want %+v", decodedCfg, cfg)
		}
	})
}
