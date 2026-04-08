package wanf

import (
	"bytes"
	"reflect"
	"strings"
	"testing"
	"time"
)

func TestStreamDecoderRegression_CanDecodeSingleLineEncoderOutput(t *testing.T) {
	want := regressionConfig{
		Enabled: true,
		Server:  regressionServer{Port: 8080},
		IDs:     []int{1, 2, 3},
	}

	var buf bytes.Buffer
	enc := NewEncoder(&buf, WithStyle(StyleSingleLine))
	if err := enc.Encode(want); err != nil {
		t.Fatalf("Encode failed: %v", err)
	}

	dec, err := NewStreamDecoder(strings.NewReader(buf.String()))
	if err != nil {
		t.Fatalf("NewStreamDecoder failed: %v", err)
	}

	var got regressionConfig
	if err := dec.Decode(&got); err != nil {
		t.Fatalf("stream decoder should accept single-line encoder output, got: %v", err)
	}
	if !reflect.DeepEqual(got, want) {
		t.Fatalf("stream decode mismatch. got=%+v want=%+v", got, want)
	}
}

func TestStreamDecoderRegression_EnvCallWithoutWhitespaceSucceeds(t *testing.T) {
	input := `secrets { timeout = env("TIMEOUT", "15s") }`
	var cfg struct {
		Secrets struct {
			Timeout time.Duration `wanf:"timeout"`
		} `wanf:"secrets"`
	}

	dec, err := NewStreamDecoder(strings.NewReader(input))
	if err != nil {
		t.Fatalf("NewStreamDecoder failed: %v", err)
	}
	if err := dec.Decode(&cfg); err != nil {
		t.Fatalf("stream decoder should parse env(...) without extra whitespace, got: %v", err)
	}
	if cfg.Secrets.Timeout != 15*time.Second {
		t.Fatalf("timeout mismatch: got %v want %v", cfg.Secrets.Timeout, 15*time.Second)
	}
}
