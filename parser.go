package wanf

import (
	"bytes"
	"fmt"
	"os"
	"strconv"
)

const (
	_ int = iota
	LOWEST
)

type (
	prefixParseFn func() Expression
)

type ErrorLevel int

const (
	ErrorLevelLint ErrorLevel = iota
	ErrorLevelFmt
)

func (el ErrorLevel) String() string {
	switch el {
	case ErrorLevelLint:
		return "LINT"
	case ErrorLevelFmt:
		return "FMT"
	default:
		return "UNKNOWN"
	}
}

type ErrorType int

const (
	ErrUnknown ErrorType = iota
	ErrUnexpectedToken
	ErrRedundantComma
	ErrRedundantLabel
	ErrUnusedVariable
	ErrExpectDiffToken
	ErrMissingComma
)

type LintError struct {
	Line      int        `json:"line"`
	Column    int        `json:"column"`
	EndLine   int        `json:"endLine"`
	EndColumn int        `json:"endColumn"`
	Message   string     `json:"message"`
	Level     ErrorLevel `json:"level"`
	Type      ErrorType  `json:"type"`
	Args      []string   `json:"args,omitempty"`
}

func (e LintError) Error() string {
	return fmt.Sprintf("line %d:%d: %s", e.Line, e.Column, e.Message)
}

type Parser struct {
	l              lexer
	errors         []LintError
	curToken       Token
	peekToken      Token
	prefixParseFns map[TokenType]prefixParseFn
	LintMode       bool
	lintErrors     []LintError
}

func NewParser(l lexer) *Parser {
	p := &Parser{
		l:          l,
		errors:     []LintError{},
		lintErrors: []LintError{},
	}
	p.prefixParseFns = make(map[TokenType]prefixParseFn)
	p.registerPrefix(IDENT, p.parseIdentifier)
	p.registerPrefix(INT, p.parseIntegerLiteral)
	p.registerPrefix(FLOAT, p.parseFloatLiteral)
	p.registerPrefix(STRING, p.parseStringLiteral)
	p.registerPrefix(BOOL, p.parseBooleanLiteral)
	p.registerPrefix(DUR, p.parseDurationLiteral)
	p.registerPrefix(LBRACK, p.parseListLiteral)
	p.registerPrefix(LBRACE, p.parseBlockOrMapLiteral)
	p.registerPrefix(DOLLAR_LBRACE, p.parseVarExpression)
	p.nextToken()
	p.nextToken()
	return p
}

func (p *Parser) Errors() []LintError {
	return p.errors
}
func (p *Parser) SetLintMode(enabled bool) {
	p.LintMode = enabled
}
func (p *Parser) LintErrors() []LintError {
	return p.lintErrors
}
func (p *Parser) nextToken() {
	p.curToken = p.peekToken
	p.peekToken = p.l.NextToken()
}

func (p *Parser) ParseProgram() *RootNode {
	program := &RootNode{}
	program.Statements = []Statement{}
	for !p.curTokenIs(EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			program.Statements = append(program.Statements, stmt)
		}
	}
	return program
}

func (p *Parser) parseLeadingComments() []*Comment {
	var comments []*Comment
	for p.curTokenIs(COMMENT) {
		comment := &Comment{Token: p.curToken, Text: p.curToken.Literal}
		comments = append(comments, comment)
		p.nextToken()
	}
	return comments
}

func (p *Parser) parseStatement() Statement {
	leadingComments := p.parseLeadingComments()

	if p.curTokenIs(EOF) {
		return nil
	}

	var stmt Statement
	switch p.curToken.Type {
	case SEMICOLON:
		p.nextToken()
		return nil
	case VAR:
		stmt = p.parseVarStatement(leadingComments)
	case IMPORT:
		stmt = p.parseImportStatement(leadingComments)
	case IDENT:
		if p.peekTokenIs(ASSIGN) {
			stmt = p.parseAssignStatement(leadingComments)
		} else if p.peekTokenIs(LBRACE) || p.peekTokenIs(STRING) {
			stmt = p.parseBlockStatement(leadingComments)
		}
	}

	if stmt == nil {
		if p.LintMode {
			message := fmt.Sprintf("unexpected token %s (%s)", p.curToken.Type, string(p.curToken.Literal))
			if p.curToken.Type == ILLEGAL {
				message = string(p.curToken.Literal)
			}
			var args []string
			if p.curToken.Type != ILLEGAL {
				args = []string{string(p.curToken.Type), string(p.curToken.Literal)}
			}
			p.lintErrors = append(p.lintErrors, LintError{
				Line:      p.curToken.Line,
				Column:    p.curToken.Column,
				EndLine:   p.curToken.Line,
				EndColumn: p.curToken.Column + len(p.curToken.Literal),
				Message:   message,
				Level:     ErrorLevelLint,
				Type:      ErrUnexpectedToken,
				Args:      args,
			})
		} else {
			p.appendError(fmt.Sprintf("unexpected token %s (%s)", p.curToken.Type, string(p.curToken.Literal)))
		}
		p.nextToken()
		return nil
	}

	if p.peekTokenIs(COMMENT) && p.peekToken.Line == p.curToken.Line {
		p.nextToken()
		lineComment := &Comment{Token: p.curToken, Text: p.curToken.Literal}
		switch s := stmt.(type) {
		case *AssignStatement:
			s.LineComment = lineComment
		case *VarStatement:
			s.LineComment = lineComment
		case *ImportStatement:
			s.LineComment = lineComment
		}
	}

	p.nextToken()
	return stmt
}

func (p *Parser) parseAssignStatement(leading []*Comment) *AssignStatement {
	stmt := &AssignStatement{Token: p.curToken, LeadingComments: leading}
	stmt.Name = &Identifier{Token: p.curToken, Value: p.curToken.Literal}
	p.nextToken()
	p.nextToken()
	stmt.Value = p.parseExpression(LOWEST)
	return stmt
}

func (p *Parser) parseBlockStatement(leading []*Comment) *BlockStatement {
	stmt := &BlockStatement{Token: p.curToken, LeadingComments: leading}
	stmt.Name = &Identifier{Token: p.curToken, Value: p.curToken.Literal}
	if p.peekTokenIs(STRING) {
		p.nextToken()
		stmt.Label = p.parseStringLiteral().(*StringLiteral)
	}
	if !p.expectPeek(LBRACE) {
		return nil
	}
	stmt.Body = p.parseBlockBody()
	return stmt
}

func (p *Parser) parseBlockBody() *RootNode {
	body := &RootNode{}
	body.Statements = []Statement{}
	p.nextToken()
	for !p.curTokenIs(RBRACE) && !p.curTokenIs(EOF) {
		stmt := p.parseStatement()
		if stmt != nil {
			body.Statements = append(body.Statements, stmt)
		}
		if p.curTokenIs(COMMA) {
			p.lintErrors = append(p.lintErrors, LintError{
				Line:      p.curToken.Line,
				Column:    p.curToken.Column,
				EndLine:   p.curToken.Line,
				EndColumn: p.curToken.Column + len(p.curToken.Literal),
				Message:   "redundant comma; statements in a block should be separated by newlines",
				Level:     ErrorLevelFmt,
				Type:      ErrRedundantComma,
			})
			p.nextToken()
		}
	}
	return body
}

func (p *Parser) parseVarStatement(leading []*Comment) *VarStatement {
	stmt := &VarStatement{Token: p.curToken, LeadingComments: leading}
	if !p.expectPeek(IDENT) {
		return nil
	}
	stmt.Name = &Identifier{Token: p.curToken, Value: p.curToken.Literal}
	if !p.expectPeek(ASSIGN) {
		return nil
	}
	p.nextToken()
	stmt.Value = p.parseExpression(LOWEST)
	return stmt
}

func (p *Parser) parseImportStatement(leading []*Comment) *ImportStatement {
	stmt := &ImportStatement{Token: p.curToken, LeadingComments: leading}
	if !p.expectPeek(STRING) {
		return nil
	}
	stmt.Path = p.parseStringLiteral().(*StringLiteral)
	return stmt
}

func (p *Parser) parseExpression(precedence int) Expression {
	prefix := p.prefixParseFns[p.curToken.Type]
	if prefix == nil {
		p.noPrefixParseFnError(p.curToken.Type)
		return nil
	}
	leftExp := prefix()
	return leftExp
}

var envLiteral = []byte("env")

func (p *Parser) parseIdentifier() Expression {
	if bytes.Equal(p.curToken.Literal, envLiteral) && p.peekTokenIs(LPAREN) {
		return p.parseEnvExpression()
	}
	return &Identifier{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseIntegerLiteral() Expression {
	lit := &IntegerLiteral{Token: p.curToken}
	value, err := strconv.ParseInt(BytesToString(p.curToken.Literal), 0, 64)
	if err != nil {
		p.appendError(fmt.Sprintf("could not parse %q as integer", p.curToken.Literal))
		return nil
	}
	lit.Value = value
	return lit
}

func (p *Parser) parseFloatLiteral() Expression {
	lit := &FloatLiteral{Token: p.curToken}
	value, err := strconv.ParseFloat(BytesToString(p.curToken.Literal), 64)
	if err != nil {
		p.appendError(fmt.Sprintf("could not parse %q as float", p.curToken.Literal))
		return nil
	}
	lit.Value = value
	return lit
}

func (p *Parser) parseStringLiteral() Expression {
	return &StringLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseBooleanLiteral() Expression {
	lit := &BoolLiteral{Token: p.curToken}
	value, err := strconv.ParseBool(BytesToString(p.curToken.Literal))
	if err != nil {
		p.appendError(fmt.Sprintf("could not parse %q as boolean", p.curToken.Literal))
		return nil
	}
	lit.Value = value
	return lit
}

func (p *Parser) parseDurationLiteral() Expression {
	return &DurationLiteral{Token: p.curToken, Value: p.curToken.Literal}
}

func (p *Parser) parseListLiteral() Expression {
	list := &ListLiteral{Token: p.curToken}
	p.nextToken()
	list.Elements = p.parseExpressionList(RBRACK)
	return list
}

func (p *Parser) parseBlockOrMapLiteral() Expression {
	if p.peekTokenIs(LBRACK) {
		return p.parseMapLiteral()
	}
	return p.parseBlockLiteral()
}

func (p *Parser) parseMapLiteral() Expression {
	mapLit := &MapLiteral{Token: p.curToken} // cur is {
	p.nextToken()                            // consume {, cur is [
	p.nextToken()                            // consume [, cur is first element

	mapLit.Elements = p.parseMapElementList()
	if mapLit.Elements == nil {
		return nil
	}

	// after parseMapElementList, curToken is RBRACK
	if !p.expectPeek(RBRACE) {
		return nil
	}
	return mapLit
}

func (p *Parser) parseMapElementList() []Statement {
	var elements []Statement

	if p.curTokenIs(RBRACK) {
		return elements
	}

	for {
		stmt := p.parseStatement()
		if stmt == nil {
			// A fatal error occurred in parseStatement, abort.
			return nil
		}
		elements = append(elements, stmt)

		if p.curTokenIs(RBRACK) {
			break // End of list
		}

		if p.curTokenIs(COMMA) {
			p.nextToken() // Consume comma
			if p.curTokenIs(RBRACK) {
				break // Trailing comma
			}
		} else {
			// Error recovery: comma is missing.
			// Log a warning and proceed as if a comma was there.
			msg := fmt.Sprintf("missing comma, auto-inserted before %s", p.curToken.Type)
			p.lintErrors = append(p.lintErrors, LintError{
				Line:      p.curToken.Line,
				Column:    p.curToken.Column,
				EndLine:   p.curToken.Line,
				EndColumn: p.curToken.Column + 1, // Highlight just the position before the token
				Message:   msg,
				Level:     ErrorLevelFmt,
				Type:      ErrMissingComma,
				Args:      []string{string(p.curToken.Type)},
			})
		}
	}

	return elements
}

func (p *Parser) parseBlockLiteral() Expression {
	block := &BlockLiteral{Token: p.curToken}
	block.Body = p.parseBlockBody()
	return block
}

func (p *Parser) parseVarExpression() Expression {
	expr := &VarExpression{Token: p.curToken}
	if !p.expectPeek(IDENT) {
		return nil
	}
	expr.Name = p.curToken.Literal
	if !p.expectPeek(RBRACE) {
		return nil
	}
	return expr
}

func (p *Parser) parseEnvExpression() Expression {
	expr := &EnvExpression{Token: p.curToken}
	if !p.expectPeek(LPAREN) {
		return nil
	}
	p.nextToken()
	if !p.curTokenIs(STRING) {
		p.appendError("expected string argument for env()")
		return nil
	}
	expr.Name = p.parseStringLiteral().(*StringLiteral)
	if p.peekTokenIs(COMMA) {
		p.nextToken()
		p.nextToken()
		if !p.curTokenIs(STRING) {
			p.appendError("expected string for env() default value")
			return nil
		}
		expr.DefaultValue = p.parseStringLiteral().(*StringLiteral)
	}
	if !p.expectPeek(RPAREN) {
		return nil
	}
	return expr
}

func (p *Parser) parseExpressionList(end TokenType) []Expression {
	var list []Expression
	if p.curTokenIs(end) {
		return list
	}
	list = append(list, p.parseExpression(LOWEST))
	for p.peekTokenIs(COMMA) {
		p.nextToken()
		p.nextToken()
		if p.curTokenIs(end) {
			break
		}
		list = append(list, p.parseExpression(LOWEST))
	}
	if !p.curTokenIs(end) {
		p.expectPeek(end)
	}
	return list
}

func (p *Parser) curTokenIs(t TokenType) bool {
	return p.curToken.Type == t
}
func (p *Parser) peekTokenIs(t TokenType) bool {
	return p.peekToken.Type == t
}
func (p *Parser) expectPeek(t TokenType) bool {
	if p.peekTokenIs(t) {
		p.nextToken()
		return true
	}
	p.peekError(t)
	return false
}
func (p *Parser) peekError(t TokenType) {
	msg := fmt.Sprintf("expected next token to be %s, got %s instead", t, p.peekToken.Type)
	p.appendErrorAt(p.peekToken, msg)
}
func (p *Parser) noPrefixParseFnError(t TokenType) {
	p.appendError(fmt.Sprintf("no prefix parse function for %s found", t))
}

func (p *Parser) appendError(msg string) {
	p.appendErrorAt(p.curToken, msg)
}

func (p *Parser) appendErrorAt(tok Token, msg string) {
	p.errors = append(p.errors, LintError{
		Line:      tok.Line,
		Column:    tok.Column,
		EndLine:   tok.Line,
		EndColumn: tok.Column + len(tok.Literal),
		Message:   "parser error: " + msg,
		Level:     ErrorLevelLint,
		Type:      ErrUnexpectedToken,
	})
}

func (p *Parser) registerPrefix(tokenType TokenType, fn prefixParseFn) {
	p.prefixParseFns[tokenType] = fn
}

// logToken 是一个调试辅助函数, 用于打印当前的 token 信息.
func (p *Parser) logToken(msg string) {
	// 你可以把 os.Stderr 换成你喜欢的日志库, 比如 log.Printf
	// 我们使用 Stderr 以免和程序本身的 stdout 混淆
	fmt.Fprintf(os.Stderr, "[DEBUGGER] %s: curToken={Type: %s, Literal: '%s'}\n",
		msg,
		p.curToken.Type,
		string(p.curToken.Literal),
	)
}
