package wanf

import (
	"bytes"
	"unicode"
)

var singleCharByteSlices [256][]byte

func init() {
	for i := 0; i < 256; i++ {
		singleCharByteSlices[i] = []byte{byte(i)}
	}
}

type Lexer struct {
	input        []byte // 使用 []byte 避免复制
	position     int
	readPosition int
	ch           byte
	line         int
	column       int
}

func NewLexer(input []byte) *Lexer {
	l := &Lexer{input: input, line: 1}
	l.readChar()
	return l
}

func (l *Lexer) readChar() {
	if l.readPosition >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.readPosition]
	}
	l.position = l.readPosition
	l.readPosition++
	l.column++
}

func (l *Lexer) NextToken() Token {
	var tok Token
	l.skipWhitespace()
	line, col := l.line, l.column
	switch l.ch {
	case '=':
		tok = l.newToken(ASSIGN, l.ch, line, col)
	case ',':
		tok = l.newToken(COMMA, l.ch, line, col)
	case ';':
		tok = l.newToken(SEMICOLON, l.ch, line, col)
	case '{':
		tok = l.newToken(LBRACE, l.ch, line, col)
	case '}':
		tok = l.newToken(RBRACE, l.ch, line, col)
	case '[':
		tok = l.newToken(LBRACK, l.ch, line, col)
	case ']':
		tok = l.newToken(RBRACK, l.ch, line, col)
	case '(':
		tok = l.newToken(LPAREN, l.ch, line, col)
	case ')':
		tok = l.newToken(RPAREN, l.ch, line, col)
	case '#':
		tok.Type = ILLEGAL_COMMENT
		tok.Literal = l.readUntilEndOfLine()
		tok.Line = line
		tok.Column = col
		return tok
	case '$':
		if l.peekChar() == '{' {
			l.readChar()
			tok = Token{Type: DOLLAR_LBRACE, Literal: []byte("${"), Line: line, Column: col}
		} else {
			tok = l.newToken(ILLEGAL, l.ch, line, col)
		}
	case '"', '\'', '`':
		tok.Type = STRING
		tok.Literal = l.readString()
		tok.Line = line
		tok.Column = col
		return tok
	case '/':
		if l.peekChar() == '/' {
			tok.Type = COMMENT
			tok.Literal = l.readSingleLineComment()
			tok.Line = line
			tok.Column = col
		} else if l.peekChar() == '*' {
			literal, ok := l.readMultiLineComment()
			if !ok {
				tok.Type = ILLEGAL
				tok.Literal = []byte("unclosed block comment")
			} else {
				tok.Type = COMMENT
				tok.Literal = literal
			}
			tok.Line = line
			tok.Column = col
		} else {
			tok = l.newToken(ILLEGAL, l.ch, line, col)
			l.readChar()
		}
		return tok
	case 0:
		tok.Literal = []byte{}
		tok.Type = EOF
		l.readChar()
		return tok
	default:
		if isIdentifierStart(l.ch) {
			literal := l.readIdentifier()
			tok.Type = LookupIdentifier(literal)
			tok.Literal = literal
			tok.Line = line
			tok.Column = col
			return tok
		} else if l.ch >= '0' && l.ch <= '9' {
			startPos := l.position
			literal := l.readNumber()
			if isDurationUnit(l.ch, l.peekChar()) {
				for {
					l.readDurationSuffix()
					if !((l.ch >= '0' && l.ch <= '9') || l.ch == '.') {
						break
					}
					numLen, hasUnit := l.peekNumberWithUnit()
					if !hasUnit {
						break
					}
					l.skipBytes(numLen)
				}
				tok.Type = DUR
				tok.Literal = l.input[startPos:l.position]
			} else {
				if bytes.Contains(literal, []byte(".")) {
					tok.Type = FLOAT
				} else {
					tok.Type = INT
				}
				tok.Literal = literal
			}
			tok.Line = line
			tok.Column = col
			return tok
		} else {
			tok = l.newToken(ILLEGAL, l.ch, line, col)
		}
	}
	l.readChar()
	return tok
}

func (l *Lexer) readDurationSuffix() {
	if l.ch == 'm' || l.ch == 'u' || l.ch == 'n' {
		if l.peekChar() == 's' {
			l.readChar()
		}
	} else if l.ch == 0xC2 && l.peekChar() == 0xB5 {
		l.readChar()
		if l.peekChar() == 's' {
			l.readChar()
		}
	}
	l.readChar()
}

func (l *Lexer) peekNumberWithUnit() (int, bool) {
	p := l.position
	isFloat := false
	start := p
	for p < len(l.input) {
		ch := l.input[p]
		if ch >= '0' && ch <= '9' {
			p++
		} else if ch == '.' && !isFloat {
			isFloat = true
			p++
		} else {
			break
		}
	}

	if p == start || p >= len(l.input) {
		return 0, false
	}

	var next byte
	if p+1 < len(l.input) {
		next = l.input[p+1]
	}
	if isDurationUnit(l.input[p], next) {
		return p - start, true
	}
	return 0, false
}

func (l *Lexer) skipBytes(n int) {
	l.position += n
	l.readPosition = l.position + 1
	l.column += n
	if l.position >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[l.position]
	}
}
func (l *Lexer) skipWhitespace() {
	for l.ch == ' ' || l.ch == '\t' || l.ch == '\r' || l.ch == '\n' {
		if l.ch == '\n' {
			l.line++
			l.column = 0
		}
		l.readChar()
	}
}
func (l *Lexer) readSingleLineComment() []byte {
	position := l.position
	for l.ch != '\n' && l.ch != 0 {
		l.readChar()
	}
	return l.input[position:l.position]
}

func (l *Lexer) readMultiLineComment() ([]byte, bool) {
	position := l.position
	l.readChar() // consume '/'
	l.readChar() // consume '*'
	for {
		if l.ch == 0 {
			return l.input[position:l.position], false // unclosed
		}
		if l.ch == '*' && l.peekChar() == '/' {
			l.readChar()
			l.readChar()
			break // closed
		}
		if l.ch == '\n' {
			l.line++
			l.column = 0
		}
		l.readChar()
	}
	return l.input[position:l.position], true // closed
}

func (l *Lexer) readIdentifier() []byte {
	position := l.position
	for isIdentifierChar(l.ch) {
		l.readChar()
	}
	return l.input[position:l.position]
}
func (l *Lexer) readNumber() []byte {
	position := l.position
	isFloat := false
	for (l.ch >= '0' && l.ch <= '9') || (l.ch == '.' && !isFloat) {
		if l.ch == '.' {
			isFloat = true
		}
		l.readChar()
	}
	return l.input[position:l.position]
}
func (l *Lexer) readString() []byte {
	quote := l.ch
	position := l.position + 1
	for {
		l.readChar()
		if l.ch == quote || l.ch == 0 {
			break
		}
	}
	literal := l.input[position:l.position]
	l.readChar()
	return literal
}

func (l *Lexer) readUntilEndOfLine() []byte {
	position := l.position
	for {
		if l.ch == '\n' || l.ch == '\r' || l.ch == 0 {
			break
		}
		l.readChar()
	}
	return l.input[position:l.position]
}
func (l *Lexer) peekChar() byte {
	if l.readPosition >= len(l.input) {
		return 0
	}
	return l.input[l.readPosition]
}
func (l *Lexer) newToken(tokenType TokenType, ch byte, line, column int) Token {
	return Token{Type: tokenType, Literal: singleCharByteSlices[ch], Line: line, Column: column}
}
func isDurationUnit(ch byte, next byte) bool {
	switch ch {
	case 'h', 'm', 's':
		return true
	case 'u', 'n':
		return next == 's'
	case 0xC2: // UTF-8 for µ is 0xC2 0xB5
		return next == 0xB5
	}
	return false
}

func isIdentifierStart(ch byte) bool {
	return (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || ch == '_'
}
func isIdentifierChar(ch byte) bool {
	r := rune(ch) // 将byte转换为rune，以便使用unicode包的函数

	if unicode.IsLetter(r) || unicode.IsDigit(r) || ch == '_' {
		return true
	}

	// 排除 WANF 中的分隔符和特殊符号，这些不能作为标识符的一部分
	switch ch {
	case '=', ',', ';', '{', '}', '[', ']', '(', ')', '"', '\'', '`', '/', '*', '$', '#':
		return false
	}

	return unicode.IsPunct(r) || // 检查是否为Unicode标点符号
		unicode.IsSymbol(r) // 检查是否为Unicode通用符号
}
