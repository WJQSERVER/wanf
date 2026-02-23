package wanf

import (
	"bytes"
	"testing"
)

func TestParseProgram(t *testing.T) {
	input := `
var version = 1.0
import "common.wanf"

app {
    name = "Test App"
}

server "main" {
    host = env("HOST", "localhost")
    port = 8080
}
`
	l := NewLexer([]byte(input))
	p := NewParser(l)
	program := p.ParseProgram()

	if program == nil {
		t.Fatalf("ParseProgram() returned nil")
	}
	if len(program.Statements) != 4 {
		t.Fatalf("program.Statements does not contain 4 statements. got=%d", len(program.Statements))
	}

	checkParserErrors(t, p)

	// Test Var Statement
	varStmt, ok := program.Statements[0].(*VarStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not *VarStatement. got=%T", program.Statements[0])
	}
	if !bytes.Equal(varStmt.Name.Value, []byte("version")) {
		t.Fatalf("varStmt.Name.Value not 'version'. got=%s", varStmt.Name.Value)
	}
	if _, ok := varStmt.Value.(*FloatLiteral); !ok {
		t.Fatalf("varStmt.Value is not *FloatLiteral. got=%T", varStmt.Value)
	}

	// Test Import Statement
	importStmt, ok := program.Statements[1].(*ImportStatement)
	if !ok {
		t.Fatalf("program.Statements[1] is not *ImportStatement. got=%T", program.Statements[1])
	}
	if !bytes.Equal(importStmt.Path.Value, []byte("common.wanf")) {
		t.Fatalf("importStmt.Path.Value not 'common.wanf'. got=%s", importStmt.Path.Value)
	}

	// Test Block Statement (unlabeled)
	blockStmt1, ok := program.Statements[2].(*BlockStatement)
	if !ok {
		t.Fatalf("program.Statements[2] is not *BlockStatement. got=%T", program.Statements[2])
	}
	if !bytes.Equal(blockStmt1.Name.Value, []byte("app")) {
		t.Fatalf("blockStmt1.Name.Value not 'app'. got=%s", blockStmt1.Name.Value)
	}
	if blockStmt1.Label != nil {
		t.Fatalf("blockStmt1.Label should be nil. got=%v", blockStmt1.Label)
	}

	// Test Block Statement (labeled)
	blockStmt2, ok := program.Statements[3].(*BlockStatement)
	if !ok {
		t.Fatalf("program.Statements[3] is not *BlockStatement. got=%T", program.Statements[3])
	}
	if !bytes.Equal(blockStmt2.Name.Value, []byte("server")) {
		t.Fatalf("blockStmt2.Name.Value not 'server'. got=%s", blockStmt2.Name.Value)
	}
	if blockStmt2.Label == nil || !bytes.Equal(blockStmt2.Label.Value, []byte("main")) {
		t.Fatalf("blockStmt2.Label.Value not 'main'. got=%v", blockStmt2.Label)
	}

	// Further checks inside blocks can be added here
}

func TestParseComments(t *testing.T) {
	input := `
// This is a leading comment.
// It has two lines.
key = "value" // This is a line comment.
`
	l := NewLexer([]byte(input))
	p := NewParser(l)
	program := p.ParseProgram()
	checkParserErrors(t, p)

	if len(program.Statements) != 1 {
		t.Fatalf("program.Statements does not contain 1 statement. got=%d", len(program.Statements))
	}

	stmt, ok := program.Statements[0].(*AssignStatement)
	if !ok {
		t.Fatalf("program.Statements[0] is not *AssignStatement. got=%T", program.Statements[0])
	}

	if len(stmt.LeadingComments) != 2 {
		t.Fatalf("stmt.LeadingComments does not contain 2 comments. got=%d", len(stmt.LeadingComments))
	}

	if !bytes.Equal(stmt.LeadingComments[0].Text, []byte("// This is a leading comment.")) {
		t.Errorf("stmt.LeadingComments[0].Text wrong. got=%q", stmt.LeadingComments[0].Text)
	}
	if !bytes.Equal(stmt.LeadingComments[1].Text, []byte("// It has two lines.")) {
		t.Errorf("stmt.LeadingComments[1].Text wrong. got=%q", stmt.LeadingComments[1].Text)
	}

	if stmt.LineComment == nil {
		t.Fatalf("stmt.LineComment is nil")
	}

	if !bytes.Equal(stmt.LineComment.Text, []byte("// This is a line comment.")) {
		t.Errorf("stmt.LineComment.Text wrong. got=%q", stmt.LineComment.Text)
	}
}

func checkParserErrors(t *testing.T, p *Parser) {
	errors := p.Errors()
	if len(errors) == 0 {
		return
	}

	t.Errorf("parser has %d errors", len(errors))
	for _, msg := range errors {
		t.Errorf("parser error: %q", msg)
	}
	t.FailNow()
}
