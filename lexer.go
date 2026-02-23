package wanf

import (
	"unicode"
)

var singleCharByteSlices [256][]byte
var isIdentTable [256]bool
var tokenDollarLbraceLiteral = []byte("${")
var unclosedBlockCommentLiteral = []byte("unclosed block comment")

func init() {
	for i := range 256 {
		singleCharByteSlices[i] = []byte{byte(i)}
		ch := byte(i)
		if (ch >= 'a' && ch <= 'z') || (ch >= 'A' && ch <= 'Z') || (ch >= '0' && ch <= '9') || ch == '_' {
			isIdentTable[i] = true
		}
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

func putLexer(l *Lexer) {
	// Lexer 已经不再使用池化，保持此函数为空以兼容已有的 Decode 实现
}

func (l *Lexer) readChar() {
	p := l.readPosition
	if p >= len(l.input) {
		l.ch = 0
		l.position = p
		l.readPosition = p + 1
	} else {
		l.ch = l.input[p]
		l.position = p
		l.readPosition = p + 1
	}
	l.column++
}

func (l *Lexer) IsPersistent() bool {
	return true
}

func (l *Lexer) NextToken() Token {
	var tok Token
	l.skipWhitespace()
	line, col := l.line, l.column
	switch l.ch {
	case '=':
		tok = Token{Type: ASSIGN, Literal: singleCharByteSlices['='], Line: line, Column: col}
		l.readChar()
		return tok
	case ',':
		tok = Token{Type: COMMA, Literal: singleCharByteSlices[','], Line: line, Column: col}
		l.readChar()
		return tok
	case ';':
		tok = Token{Type: SEMICOLON, Literal: singleCharByteSlices[';'], Line: line, Column: col}
		l.readChar()
		return tok
	case '{':
		tok = Token{Type: LBRACE, Literal: singleCharByteSlices['{'], Line: line, Column: col}
		l.readChar()
		return tok
	case '}':
		tok = Token{Type: RBRACE, Literal: singleCharByteSlices['}'], Line: line, Column: col}
		l.readChar()
		return tok
	case '[':
		tok = Token{Type: LBRACK, Literal: singleCharByteSlices['['], Line: line, Column: col}
		l.readChar()
		return tok
	case ']':
		tok = Token{Type: RBRACK, Literal: singleCharByteSlices[']'], Line: line, Column: col}
		l.readChar()
		return tok
	case '(':
		tok = Token{Type: LPAREN, Literal: singleCharByteSlices['('], Line: line, Column: col}
		l.readChar()
		return tok
	case ')':
		tok = Token{Type: RPAREN, Literal: singleCharByteSlices[')'], Line: line, Column: col}
		l.readChar()
		return tok
	case '#':
		tok.Type = ILLEGAL_COMMENT
		tok.Literal = l.readUntilEndOfLine()
		tok.Line = line
		tok.Column = col
		return tok
	case '$':
		if l.peekChar() == '{' {
			l.readChar()
			tok = Token{Type: DOLLAR_LBRACE, Literal: tokenDollarLbraceLiteral, Line: line, Column: col}
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
				tok.Literal = unclosedBlockCommentLiteral
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
			isFloat := false
			p := l.position
			for p < len(l.input) {
				c := l.input[p]
				if c >= '0' && c <= '9' {
					p++
				} else if c == '.' && !isFloat {
					isFloat = true
					p++
				} else {
					break
				}
			}
			l.skipBytes(p - l.position)
			if isDurationUnit(l.ch, l.peekChar()) {
				for {
					l.readDurationSuffix()
					if !((l.ch >= '0' && l.ch <= '9') || l.ch == '.') {
						break
					}
					p = l.position
					for p < len(l.input) {
						c := l.input[p]
						if (c >= '0' && c <= '9') || c == '.' {
							p++
						} else {
							break
						}
					}
					numLen := p - l.position
					var next byte
					if p < len(l.input) {
						next = l.input[p]
					}
					var next2 byte
					if p+1 < len(l.input) {
						next2 = l.input[p+1]
					}
					if isDurationUnit(next, next2) {
						l.skipBytes(numLen)
					} else {
						break
					}
				}
				tok.Type = DUR
				tok.Literal = l.input[startPos:l.position]
			} else {
				if isFloat {
					tok.Type = FLOAT
				} else {
					tok.Type = INT
				}
				tok.Literal = l.input[startPos:l.position]
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

func (l *Lexer) skipBytes(n int) {
	l.position += n
	l.readPosition = l.position + 1
	l.column += n
	p := l.position
	if p >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[p]
	}
}
func (l *Lexer) skipWhitespace() {
	p := l.position
	for p < len(l.input) {
		ch := l.input[p]
		switch ch {
		case ' ', '\t', '\r':
			p++
			l.column++
		case '\n':
			p++
			l.line++
			l.column = 0
		default:
			l.position = p
			l.readPosition = p + 1
			l.ch = ch
			return
		}
	}
	l.position = p
	l.readPosition = p + 1
	l.ch = 0
}
func (l *Lexer) readSingleLineComment() []byte {
	start := l.position
	p := l.position
	for p < len(l.input) && l.input[p] != '\n' {
		p++
	}
	l.skipBytes(p - l.position)
	return l.input[start:p]
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
	start := l.position
	p := l.position
	for p < len(l.input) {
		ch := l.input[p]
		if ch < 128 {
			if !isIdentTable[ch] {
				break
			}
		} else {
			if !isIdentifierCharNonASCII(ch) {
				break
			}
		}
		p++
	}

	n := p - start
	l.position = p
	l.readPosition = p + 1
	l.column += n
	if p >= len(l.input) {
		l.ch = 0
	} else {
		l.ch = l.input[p]
	}
	return l.input[start:p]
}
func (l *Lexer) readString() []byte {
	quote := l.ch
	p := l.readPosition
	start := p
	for p < len(l.input) && l.input[p] != quote {
		p++
	}
	literal := l.input[start:p]
	l.skipBytes(p - l.position + 1)
	return literal
}

func (l *Lexer) readUntilEndOfLine() []byte {
	start := l.position
	p := l.position
	for p < len(l.input) && l.input[p] != '\n' && l.input[p] != '\r' {
		p++
	}
	l.skipBytes(p - l.position)
	return l.input[start:p]
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
	return isIdentTable[ch]
}

func isIdentifierCharNonASCII(ch byte) bool {
	r := rune(ch)
	if unicode.IsLetter(r) || unicode.IsDigit(r) {
		return true
	}
	switch ch {
	case '=', ',', ';', '{', '}', '[', ']', '(', ')', '"', '\'', '`', '/', '*', '$', '#':
		return false
	}
	return unicode.IsPunct(r) || unicode.IsSymbol(r)
}
