package wanf

import (
	"strings"
	"testing"
)

// These tests intentionally characterize current lexer/parser behavior.
// They exist to pinpoint failures before they are observed through Decode.

func TestParserRegression_CompactSingleLineInputIsConsumedAsOneIdentifier(t *testing.T) {
	input := `enabled=true;server{port=8080};ids=[1,2,3]`

	l := NewLexer([]byte(input))
	tok := l.NextToken()
	if tok.Type != IDENT {
		t.Fatalf("expected first token to be IDENT, got %s", tok.Type)
	}
	if string(tok.Literal) != input {
		t.Fatalf("expected compact input to be tokenized as one identifier, got %q", tok.Literal)
	}

	p := NewParser(NewLexer([]byte(input)))
	_ = p.ParseProgram()
	if len(p.Errors()) == 0 {
		t.Fatal("expected parser errors for compact single-line input without spaces")
	}
	if !strings.Contains(p.Errors()[0].Error(), `unexpected token IDENT (`+input+`)`) {
		t.Fatalf("unexpected parser error: %v", p.Errors()[0])
	}
}

func TestParserRegression_EnvCallWithoutWhitespaceAfterNameBreaksParser(t *testing.T) {
	input := `server { host = env("HOST", "localhost") }`

	l := NewLexer([]byte(`env("HOST", "localhost")`))
	tok := l.NextToken()
	if tok.Type != IDENT {
		t.Fatalf("expected env(...) to start as IDENT in current lexer, got %s", tok.Type)
	}
	if string(tok.Literal) != `env("HOST",` {
		t.Fatalf("unexpected env token literal: %q", tok.Literal)
	}

	p := NewParser(NewLexer([]byte(input)))
	_ = p.ParseProgram()
	if len(p.Errors()) != 2 {
		t.Fatalf("expected 2 parser errors, got %d: %v", len(p.Errors()), p.Errors())
	}
	if !strings.Contains(p.Errors()[0].Error(), `unexpected token STRING (localhost)`) {
		t.Fatalf("unexpected first parser error: %v", p.Errors()[0])
	}
	if !strings.Contains(p.Errors()[1].Error(), `unexpected token ) ())`) {
		t.Fatalf("unexpected second parser error: %v", p.Errors()[1])
	}
}

func TestParserRegression_VarExpressionWithoutWhitespaceBeforeBraceBreaksParser(t *testing.T) {
	input := `from_import = ${shared}`

	l := NewLexer([]byte(`${shared}`))
	if tok := l.NextToken(); tok.Type != DOLLAR_LBRACE {
		t.Fatalf("expected ${ token, got %s", tok.Type)
	}
	identTok := l.NextToken()
	if identTok.Type != IDENT {
		t.Fatalf("expected IDENT after ${, got %s", identTok.Type)
	}
	if string(identTok.Literal) != `shared}` {
		t.Fatalf("expected current lexer to absorb closing brace into identifier, got %q", identTok.Literal)
	}

	p := NewParser(NewLexer([]byte(input)))
	_ = p.ParseProgram()
	if len(p.Errors()) != 1 {
		t.Fatalf("expected 1 parser error, got %d: %v", len(p.Errors()), p.Errors())
	}
	if !strings.Contains(p.Errors()[0].Error(), `expected next token to be }, got EOF instead`) {
		t.Fatalf("unexpected parser error: %v", p.Errors()[0])
	}
}
