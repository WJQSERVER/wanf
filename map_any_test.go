package wanf

import (
	"bytes"
	"strings"
	"testing"
)

func TestDecodeMapAny(t *testing.T) {
	input := `
		data {
			name = "test"
			value = 123
			nested {
				bool = true
			}
		}

		map_lit = {[
			key1 = "val1",
			key2 = 456
		]}
	`

	type Config struct {
		Data   map[string]any `wanf:"data"`
		MapLit map[string]any `wanf:"map_lit"`
	}

	var cfg Config
	dec, err := NewDecoder(strings.NewReader(input))
	if err != nil {
		t.Fatalf("failed to create decoder: %v", err)
	}

	if err := dec.Decode(&cfg); err != nil {
		t.Fatalf("failed to decode: %v", err)
	}

	// Verify Data
	if cfg.Data["name"] != "test" {
		t.Errorf("expected Data.name to be 'test', got %v", cfg.Data["name"])
	}
	if cfg.Data["value"] != int64(123) {
		t.Errorf("expected Data.value to be 123, got %v", cfg.Data["value"])
	}
	nested, ok := cfg.Data["nested"].(map[string]any)
	if !ok {
		t.Fatalf("expected Data.nested to be map[string]any, got %T", cfg.Data["nested"])
	}
	if nested["bool"] != true {
		t.Errorf("expected Data.nested.bool to be true, got %v", nested["bool"])
	}

	// Verify MapLit
	if cfg.MapLit["key1"] != "val1" {
		t.Errorf("expected MapLit.key1 to be 'val1', got %v", cfg.MapLit["key1"])
	}
	if cfg.MapLit["key2"] != int64(456) {
		t.Errorf("expected MapLit.key2 to be 456, got %v", cfg.MapLit["key2"])
	}
}

type OrdinalConfig struct {
	Strategy string `json:"strategy" wanf:"strategy"`
	Routes   []Route
}

type Route struct {
	TargetID string `json:"target_id" wanf:"target_id"`
}

type UserNode struct {
	ID      string        `json:"id" wanf:"id"`
	Ordinal OrdinalConfig `json:"ordinal" wanf:"ordinal"`
}

type UserData struct {
	Nodes map[string]UserNode `json:"nodes" wanf:"nodes"`
}

func TestUserReproduction(t *testing.T) {
	wanfData := `
nodes = {
	test = {
		id = "test"
		ordinal {
			strategy = "auto"
			routes = [
				{ target_id = "next" }
			]
		}
	}
}
`
	var data UserData
	err := Decode([]byte(wanfData), &data)
	if err != nil {
		t.Fatalf("Error: %v", err)
	}

	// Verification
	if node, ok := data.Nodes["test"]; ok {
		if node.ID != "test" {
			t.Errorf("expected node ID 'test', got %q", node.ID)
		}
		if node.Ordinal.Strategy != "auto" {
			t.Errorf("expected ordinal strategy 'auto', got %q", node.Ordinal.Strategy)
		}
		if len(node.Ordinal.Routes) != 1 {
			t.Errorf("expected 1 route, got %d", len(node.Ordinal.Routes))
		} else if node.Ordinal.Routes[0].TargetID != "next" {
			t.Errorf("expected route target_id 'next', got %q", node.Ordinal.Routes[0].TargetID)
		}
	} else {
		t.Errorf("node 'test' not found in map")
	}
}

func TestEncodeMapAny(t *testing.T) {
	type Config struct {
		Data map[string]any `wanf:"data"`
	}

	cfg := Config{
		Data: map[string]any{
			"name":  "test",
			"value": int64(123),
			"nested": map[string]any{
				"bool": true,
			},
		},
	}

	// Test standard encoder
	out, err := Marshal(&cfg)
	if err != nil {
		t.Fatalf("Marshal failed: %v", err)
	}

	t.Logf("Output:\n%s", string(out))

	// Re-decode to verify
	var cfg2 Config
	if err := Decode(out, &cfg2); err != nil {
		t.Fatalf("Decode failed: %v", err)
	}

	if cfg2.Data["name"] != "test" {
		t.Errorf("expected name 'test', got %v", cfg2.Data["name"])
	}
	if cfg2.Data["value"] != int64(123) {
		t.Errorf("expected value 123, got %v", cfg2.Data["value"])
	}

	// Test stream encoder
	var buf bytes.Buffer
	enc := NewEncoder(&buf)
	if err := enc.Encode(&cfg); err != nil {
		t.Fatalf("Stream Encode failed: %v", err)
	}

	t.Logf("Stream Output:\n%s", buf.String())

	var cfg3 Config
	if err := Decode(buf.Bytes(), &cfg3); err != nil {
		t.Fatalf("Decode stream output failed: %v", err)
	}
	if cfg3.Data["name"] != "test" {
		t.Errorf("expected name 'test' from stream, got %v", cfg3.Data["name"])
	}
}

func TestStreamDecodeMapAny(t *testing.T) {
	input := `
		data {
			name = "test"
			value = 123
			nested {
				bool = true
			}
		}

		map_lit = {[
			key1 = "val1",
			key2 = 456
		]}
	`

	type Config struct {
		Data   map[string]any `wanf:"data"`
		MapLit map[string]any `wanf:"map_lit"`
	}

	var cfg Config
	dec, err := NewStreamDecoder(strings.NewReader(input))
	if err != nil {
		t.Fatalf("failed to create stream decoder: %v", err)
	}
	defer dec.Close()

	if err := dec.Decode(&cfg); err != nil {
		t.Fatalf("failed to decode: %v", err)
	}

	// Verify Data
	if cfg.Data["name"] != "test" {
		t.Errorf("expected Data.name to be 'test', got %v", cfg.Data["name"])
	}
	if cfg.Data["value"] != int64(123) {
		t.Errorf("expected Data.value to be 123, got %v", cfg.Data["value"])
	}
	nested, ok := cfg.Data["nested"].(map[string]any)
	if !ok {
		t.Fatalf("expected Data.nested to be map[string]any, got %T", cfg.Data["nested"])
	}
	if nested["bool"] != true {
		t.Errorf("expected Data.nested.bool to be true, got %v", nested["bool"])
	}

	// Verify MapLit
	if cfg.MapLit["key1"] != "val1" {
		t.Errorf("expected MapLit.key1 to be 'val1', got %v", cfg.MapLit["key1"])
	}
	if cfg.MapLit["key2"] != int64(456) {
		t.Errorf("expected MapLit.key2 to be 456, got %v", cfg.MapLit["key2"])
	}
}
