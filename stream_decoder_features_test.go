package wanf

import (
	"os"
	"strings"
	"testing"
)

// TestStreamDecoder_VarSupport tests that the decoder correctly handles
// var statements in stream mode.
func TestStreamDecoder_VarSupport(t *testing.T) {
	wanfData := `
		var a = 123
		val = ${a}
	`
	var cfg struct {
		Val int `wanf:"val"`
	}

	r := strings.NewReader(wanfData)
	decoder, err := NewStreamDecoder(r)
	if err != nil {
		t.Fatalf("NewStreamDecoder failed: %v", err)
	}

	err = decoder.Decode(&cfg)
	if err != nil {
		t.Fatalf("Decode failed: %v", err)
	}

	if cfg.Val != 123 {
		t.Errorf("Expected val=123, got %d", cfg.Val)
	}
}

// TestStreamDecoder_ImportSupport tests that the decoder correctly handles
// import statements in stream mode.
func TestStreamDecoder_ImportSupport(t *testing.T) {
	// Create a temporary file for import
	importData := `imported_val = "hello"`
	importFile := "test_import.wanf"
	err := os.WriteFile(importFile, []byte(importData), 0644)
	if err != nil {
		t.Fatalf("Failed to create temporary file: %v", err)
	}
	defer os.Remove(importFile)

	wanfData := `import "test_import.wanf"`
	var cfg struct {
		ImportedVal string `wanf:"imported_val"`
	}

	r := strings.NewReader(wanfData)
	decoder, err := NewStreamDecoder(r, WithBasePath("."))
	if err != nil {
		t.Fatalf("NewStreamDecoder failed: %v", err)
	}

	err = decoder.Decode(&cfg)
	if err != nil {
		t.Fatalf("Decode failed: %v", err)
	}

	if cfg.ImportedVal != "hello" {
		t.Errorf("Expected imported_val='hello', got %q", cfg.ImportedVal)
	}
}
