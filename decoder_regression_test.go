package wanf

import (
	"bytes"
	"strings"
	"testing"
	"time"
)

type regressionServer struct {
	Port int `wanf:"port"`
}

type regressionConfig struct {
	Enabled bool             `wanf:"enabled"`
	Server  regressionServer `wanf:"server"`
	IDs     []int            `wanf:"ids"`
}

func TestDecoderRegression_StandardDecoderRejectsSingleLineEncoderOutput(t *testing.T) {
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

	var got regressionConfig
	err := Decode(buf.Bytes(), &got)
	if err == nil {
		t.Fatalf("expected Decode to fail for single-line encoder output %q", buf.String())
	}
	if !strings.Contains(err.Error(), `unexpected token IDENT (`) {
		t.Fatalf("expected parser-driven decode failure, got: %v", err)
	}
}

func TestDecoderRegression_StandardDecoderSucceedsWhenCompactInputAddsWhitespace(t *testing.T) {
	input := `enabled = true ; server { port = 8080 } ; ids = [1, 2, 3]`

	var got regressionConfig
	if err := Decode([]byte(input), &got); err != nil {
		t.Fatalf("Decode failed for spaced compact input: %v", err)
	}

	if !got.Enabled || got.Server.Port != 8080 || len(got.IDs) != 3 {
		t.Fatalf("decoded value mismatch: %+v", got)
	}
	if got.IDs[0] != 1 || got.IDs[1] != 2 || got.IDs[2] != 3 {
		t.Fatalf("decoded ids mismatch: %+v", got.IDs)
	}
}

func TestDecoderRegression_StandardDecoderEnvCallWithoutWhitespaceFailsBeforeMapping(t *testing.T) {
	input := `secrets { timeout = env("TIMEOUT", "15s") }`
	var cfg struct {
		Secrets struct {
			Timeout time.Duration `wanf:"timeout"`
		} `wanf:"secrets"`
	}

	err := Decode([]byte(input), &cfg)
	if err == nil {
		t.Fatal("expected Decode to fail for env(...) without whitespace after env")
	}
	if !strings.Contains(err.Error(), `unexpected token STRING (15s)`) {
		t.Fatalf("expected parser-driven env failure, got: %v", err)
	}
	if cfg.Secrets.Timeout != 0 {
		t.Fatalf("field should remain unset on parser failure, got %v", cfg.Secrets.Timeout)
	}
}
