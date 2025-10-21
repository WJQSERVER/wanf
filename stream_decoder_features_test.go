package wanf

import (
	"strings"
	"testing"
)

// TestStreamDecoder_VarError tests that the decoder correctly returns an error
// if a var statement is used in stream mode.
func TestStreamDecoder_VarError(t *testing.T) {
	wanfData := `var a = 1`
	var cfg struct{}

	r := strings.NewReader(wanfData)
	decoder, err := NewStreamDecoder(r)
	if err != nil {
		t.Fatalf("NewStreamDecoder failed: %v", err)
	}

	err = decoder.Decode(&cfg)
	if err == nil {
		t.Fatal("Expected an error for var statement, but got nil")
	}

	expectedError := "var/import statements are not supported"
	if !strings.Contains(err.Error(), expectedError) {
		t.Errorf("Expected error to contain %q, but got: %v", expectedError, err)
	}
}

// TestStreamDecoder_ImportError tests that the decoder correctly returns an error
// if an import statement is used in stream mode.
func TestStreamDecoder_ImportError(t *testing.T) {
	wanfData := `import "other.wanf"`
	var cfg struct{}

	r := strings.NewReader(wanfData)
	decoder, err := NewStreamDecoder(r)
	if err != nil {
		t.Fatalf("NewStreamDecoder failed: %v", err)
	}

	err = decoder.Decode(&cfg)
	if err == nil {
		t.Fatal("Expected an error for import statement, but got nil")
	}

	expectedError := "var/import statements are not supported"
	if !strings.Contains(err.Error(), expectedError) {
		t.Errorf("Expected error to contain %q, but got: %v", expectedError, err)
	}
}
