package wanf

import (
	"bytes"
	"fmt"
	"os"
	"path/filepath"
	"regexp"
)

var (
	varRegex = regexp.MustCompile(`\$\{(\w+)\}`)
)

func Lint(data []byte) (*RootNode, []LintError) {
	l := NewLexer(data)
	p := NewParser(l)
	p.SetLintMode(true)
	program := p.ParseProgram()
	if len(p.Errors()) > 0 {
		return program, p.Errors()
	}
	allErrors := p.LintErrors()
	analyzer := &astAnalyzer{
		errors:       allErrors,
		blockCounts:  make(map[string]int),
		declaredVars: make(map[string]*VarStatement),
		usedVars:     make(map[string]bool),
	}
	newProgram := analyzer.Analyze(program)
	return newProgram.(*RootNode), analyzer.errors
}

func Format(program *RootNode, opts FormatOptions) []byte {
	var out bytes.Buffer
	program.Format(&out, "", opts)
	return out.Bytes()
}

func DecodeFile(path string, v any) error {
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer f.Close()
	dec, err := NewDecoder(f, WithBasePath(filepath.Dir(path)))
	if err != nil {
		return err
	}
	return dec.Decode(v)
}

func Decode(data []byte, v any) error {
	if len(data) == 0 {
		return nil
	}

	// 使用池化的 StreamDecoder 逻辑
	// 但我们希望使用高性能的 Lexer 而不是 streamLexer
	l := NewLexer(data)
	defer putLexer(l)
	dec := newStreamDecoderInternal(l)
	defer putStreamDecoder(dec)

	return dec.Decode(v)
}

type astAnalyzer struct {
	errors       []LintError
	blockCounts  map[string]int
	declaredVars map[string]*VarStatement
	usedVars     map[string]bool
}

func (a *astAnalyzer) Analyze(node Node) Node {
	// First pass: collect block counts and declared variables.
	a.collect(node)

	// Second pass: check for issues.
	newNode := a.check(node)

	// Post-pass: check for unused variables.
	for name, stmt := range a.declaredVars {
		if _, ok := a.usedVars[name]; !ok {
			err := LintError{
				Line:      stmt.Token.Line,
				Column:    stmt.Token.Column,
				EndLine:   stmt.Token.Line,
				EndColumn: stmt.Token.Column + len(name),
				Message:   fmt.Sprintf("variable %q is declared but not used", name),
				Level:     ErrorLevelLint,
				Type:      ErrUnusedVariable,
				Args:      []string{name},
			}
			a.errors = append(a.errors, err)
		}
	}
	return newNode
}

func (a *astAnalyzer) collect(root Node) {
	if root == nil {
		return
	}
	stack := []Node{root}
	for len(stack) > 0 {
		node := stack[len(stack)-1]
		stack = stack[:len(stack)-1]

		if node == nil {
			continue
		}

		switch n := node.(type) {
		case *BlockStatement:
			a.blockCounts[BytesToString(n.Name.Value)]++
		case *VarStatement:
			a.declaredVars[BytesToString(n.Name.Value)] = n
		}

		switch n := node.(type) {
		case *RootNode:
			for i := len(n.Statements) - 1; i >= 0; i-- {
				stack = append(stack, n.Statements[i])
			}
		case *BlockStatement:
			stack = append(stack, n.Body)
		case *BlockLiteral:
			stack = append(stack, n.Body)
		case *AssignStatement:
			stack = append(stack, n.Value)
		case *ListLiteral:
			for i := len(n.Elements) - 1; i >= 0; i-- {
				stack = append(stack, n.Elements[i])
			}
		case *MapLiteral:
			for i := len(n.Elements) - 1; i >= 0; i-- {
				stack = append(stack, n.Elements[i])
			}
		case *VarStatement:
			stack = append(stack, n.Value)
		}
	}
}

func (a *astAnalyzer) check(node Node) Node {
	if node == nil {
		return nil
	}

	switch n := node.(type) {
	case *RootNode:
		for i, stmt := range n.Statements {
			n.Statements[i] = a.check(stmt).(Statement)
		}
		return n
	case *BlockStatement:
		if n.Body != nil {
			n.Body = a.check(n.Body).(*RootNode)
		}
		if n.Label != nil && a.blockCounts[BytesToString(n.Name.Value)] == 1 {
			err := LintError{
				Line:      n.Token.Line,
				Column:    n.Token.Column,
				EndLine:   n.Token.Line,
				EndColumn: n.Token.Column + len(n.Name.Value),
				Message:   fmt.Sprintf("block %q is defined only once, the label %q is redundant", string(n.Name.Value), string(n.Label.Value)),
				Level:     ErrorLevelFmt,
				Type:      ErrRedundantLabel,
				Args:      []string{string(n.Name.Value), string(n.Label.Value)},
			}
			a.errors = append(a.errors, err)
			return &BlockStatement{
				Token:           n.Token,
				Name:            n.Name,
				Label:           nil,
				Body:            n.Body,
				LeadingComments: n.LeadingComments,
			}
		}
		return n
	case *BlockLiteral:
		if n.Body != nil {
			n.Body = a.check(n.Body).(*RootNode)
		}
		return n
	case *AssignStatement:
		if n.Value != nil {
			n.Value = a.check(n.Value).(Expression)
		}
		return n
	case *ListLiteral:
		for i, el := range n.Elements {
			n.Elements[i] = a.check(el).(Expression)
		}
		return n
	case *MapLiteral:
		for i, st := range n.Elements {
			n.Elements[i] = a.check(st).(Statement)
		}
		return n
	case *VarStatement:
		if n.Value != nil {
			n.Value = a.check(n.Value).(Expression)
		}
		return n
	case *VarExpression:
		a.usedVars[BytesToString(n.Name)] = true
		return n
	case *StringLiteral:
		matches := varRegex.FindAllStringSubmatch(BytesToString(n.Value), -1)
		for _, match := range matches {
			if len(match) > 1 {
				a.usedVars[match[1]] = true
			}
		}
		return n
	default:
		return node
	}
}
