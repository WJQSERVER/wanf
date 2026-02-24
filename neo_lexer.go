package wanf

import (
	"io"
	"sync"
)

var (
	assignLit    = []byte("=")
	lbraceLit    = []byte("{")
	rbraceLit    = []byte("}")
	lbrackLit    = []byte("[")
	rbrackLit    = []byte("]")
	commaLit     = []byte(",")
	semicolonLit = []byte(";")
	lparenLit    = []byte("(")
	rparenLit    = []byte(")")
	colonLit     = []byte(":")
	importLit    = []byte("import")
	varLit       = []byte("var")
	dollarLbraceLit = []byte("${")
)

var isWhitespaceTable = [256]bool{
	' ':  true,
	'\t': true,
	'\n': true,
	'\r': true,
}

var singleCharTokens = [256]Token{}

func init() {
	singleCharTokens['='] = Token{Type: ASSIGN, Literal: assignLit}
	singleCharTokens['{'] = Token{Type: LBRACE, Literal: lbraceLit}
	singleCharTokens['}'] = Token{Type: RBRACE, Literal: rbraceLit}
	singleCharTokens['['] = Token{Type: LBRACK, Literal: lbrackLit}
	singleCharTokens[']'] = Token{Type: RBRACK, Literal: rbrackLit}
	singleCharTokens[','] = Token{Type: COMMA, Literal: commaLit}
	singleCharTokens[';'] = Token{Type: SEMICOLON, Literal: semicolonLit}
	singleCharTokens[':'] = Token{Type: COLON, Literal: colonLit}
	singleCharTokens['('] = Token{Type: LPAREN, Literal: lparenLit}
	singleCharTokens[')'] = Token{Type: RPAREN, Literal: rparenLit}
}

type NeoLexer struct {
	input []byte
	pos   int
	read  int
	line  int
	col   int

	reader io.Reader
	buf    []byte
	err    error

	litBuf      []byte
	isStreaming bool
}

var neoLexerPool = sync.Pool{
	New: func() any {
		return &NeoLexer{
			buf:    make([]byte, 16384),
			litBuf: make([]byte, 0, 1024),
		}
	},
}

func NewNeoLexer(r io.Reader) *NeoLexer {
	l := neoLexerPool.Get().(*NeoLexer)
	l.reader = r
	l.pos = 0
	l.read = 0
	l.line = 1
	l.col = 1
	l.err = nil
	l.input = nil
	l.litBuf = l.litBuf[:0]
	l.isStreaming = true
	return l
}

func (l *NeoLexer) SetInput(data []byte) {
	l.input = data
	l.read = len(data)
	l.pos = 0
	l.reader = nil
	l.isStreaming = false
	l.line = 1
	l.col = 1
}

func (l *NeoLexer) Close() {
	l.reader = nil
	l.input = nil
	neoLexerPool.Put(l)
}

func (l *NeoLexer) nextToken() Token {
	if !l.isStreaming {
		return l.nextTokenFast()
	}

	l.skipWhitespace()

	ch := l.peek()
	if ch == 0 {
		return Token{Type: EOF, Line: l.line, Column: l.col}
	}

	startLine, startCol := l.line, l.col

	if tok := singleCharTokens[ch]; tok.Type != "" {
		l.advance()
		tok.Line = startLine
		tok.Column = startCol
		return tok
	}

	switch ch {
	case '$':
		l.advance()
		if l.peek() == '{' {
			l.advance()
			return Token{Type: DOLLAR_LBRACE, Literal: dollarLbraceLit, Line: startLine, Column: startCol}
		}
		return Token{Type: ILLEGAL, Literal: []byte{'$'}, Line: startLine, Column: startCol}
	case '"', '\'':
		return l.readString(ch)
	case '`':
		return l.readRawString()
	default:
		if (ch >= '0' && ch <= '9') || ch == '-' || ch == '.' {
			return l.readNumberOrDuration()
		}
		if isIdentTable[ch] {
			return l.readIdentifierOrKeyword()
		}
	}
	l.advance()
	return Token{Type: ILLEGAL, Literal: []byte{ch}, Line: startLine, Column: startCol}
}

func (l *NeoLexer) nextTokenFast() Token {
	for l.pos < l.read {
		ch := l.input[l.pos]
		if isWhitespaceTable[ch] {
			if ch == '\n' {
				l.line++
				l.col = 1
			} else {
				l.col++
			}
			l.pos++
			continue
		}
		if ch == '/' && l.pos+1 < l.read {
			if l.input[l.pos+1] == '/' {
				l.pos += 2
				for l.pos < l.read && l.input[l.pos] != '\n' {
					l.pos++
				}
				continue
			} else if l.input[l.pos+1] == '*' {
				l.pos += 2
				for l.pos < l.read {
					if l.input[l.pos] == '*' && l.pos+1 < l.read && l.input[l.pos+1] == '/' {
						l.pos += 2
						break
					}
					if l.input[l.pos] == '\n' {
						l.line++
						l.col = 1
					} else {
						l.col++
					}
					l.pos++
				}
				continue
			}
		}
		break
	}

	if l.pos >= l.read {
		return Token{Type: EOF, Line: l.line, Column: l.col}
	}

	startLine, startCol := l.line, l.col
	ch := l.input[l.pos]

	if tok := singleCharTokens[ch]; tok.Type != "" {
		l.pos++
		l.col++
		tok.Line = startLine
		tok.Column = startCol
		return tok
	}

	l.pos++
	l.col++

	switch ch {
	case '$':
		if l.pos < l.read && l.input[l.pos] == '{' {
			l.pos++
			l.col++
			return Token{Type: DOLLAR_LBRACE, Literal: dollarLbraceLit, Line: startLine, Column: startCol}
		}
		return Token{Type: ILLEGAL, Literal: []byte{'$'}, Line: startLine, Column: startCol}
	case '"', '\'':
		return l.readStringFast(ch, startLine, startCol)
	case '`':
		return l.readRawStringFast(startLine, startCol)
	default:
		if (ch >= '0' && ch <= '9') || ch == '-' || ch == '.' {
			l.pos-- // backtrack
			l.col--
			return l.readNumberOrDuration()
		}
		if isIdentTable[ch] {
			l.pos-- // backtrack
			l.col--
			return l.readIdentifierOrKeyword()
		}
	}
	return Token{Type: ILLEGAL, Literal: []byte{ch}, Line: startLine, Column: startCol}
}

func (l *NeoLexer) readStringFast(quote byte, startLine, startCol int) Token {
	startPos := l.pos
	for l.pos < l.read && l.input[l.pos] != quote {
		if l.input[l.pos] == '\n' {
			l.line++
			l.col = 1
		} else {
			l.col++
		}
		l.pos++
	}
	lit := l.input[startPos:l.pos]
	if l.pos < l.read {
		l.pos++
		l.col++
	}
	return Token{Type: STRING, Literal: lit, Line: startLine, Column: startCol}
}

func (l *NeoLexer) readRawStringFast(startLine, startCol int) Token {
	startPos := l.pos
	for l.pos < l.read && l.input[l.pos] != '`' {
		if l.input[l.pos] == '\n' {
			l.line++
			l.col = 1
		} else {
			l.col++
		}
		l.pos++
	}
	lit := l.input[startPos:l.pos]
	if l.pos < l.read {
		l.pos++
		l.col++
	}
	return Token{Type: STRING, Literal: lit, Line: startLine, Column: startCol}
}

func (l *NeoLexer) peek() byte {
	if l.pos >= l.read {
		if l.reader != nil {
			n, err := l.reader.Read(l.buf)
			if n > 0 {
				l.input = l.buf
				l.read = n
				l.pos = 0
			}
			if err != nil {
				l.err = err
				if n == 0 {
					return 0
				}
			}
		} else {
			return 0
		}
	}
	return l.input[l.pos]
}

func (l *NeoLexer) advance() byte {
	ch := l.peek()
	if ch != 0 {
		l.pos++
		if ch == '\n' {
			l.line++
			l.col = 1
		} else {
			l.col++
		}
	}
	return ch
}

func (l *NeoLexer) skipWhitespace() {
	for {
		ch := l.peek()
		if isWhitespaceTable[ch] {
			l.advance()
		} else if ch == '/' {
			l.advance()
			if l.peek() == '/' {
				for l.peek() != '\n' && l.peek() != 0 {
					l.advance()
				}
			} else if l.peek() == '*' {
				l.advance()
				for {
					ch := l.advance()
					if ch == 0 || (ch == '*' && l.peek() == '/') {
						l.advance()
						break
					}
				}
			} else {
				break
			}
		} else {
			break
		}
	}
}

func (l *NeoLexer) getLiteral(startPos int, endPos int) []byte {
	if !l.isStreaming {
		return l.input[startPos:endPos]
	}
	return l.litBuf
}

func (l *NeoLexer) readIdentifierOrKeyword() Token {
	startLine, startCol := l.line, l.col
	startPos := l.pos
	l.litBuf = l.litBuf[:0]
	for isIdentTable[l.peek()] {
		ch := l.advance()
		if l.isStreaming {
			l.litBuf = append(l.litBuf, ch)
		}
	}

	lit := l.getLiteral(startPos, l.pos)

	if len(lit) == 6 && BytesToString(lit) == "import" {
		return Token{Type: IMPORT, Literal: importLit, Line: startLine, Column: startCol}
	}
	if len(lit) == 3 && BytesToString(lit) == "var" {
		return Token{Type: VAR, Literal: varLit, Line: startLine, Column: startCol}
	}
	if len(lit) == 4 && BytesToString(lit) == "true" {
		return Token{Type: BOOL, Literal: lit, Line: startLine, Column: startCol}
	}
	if len(lit) == 5 && BytesToString(lit) == "false" {
		return Token{Type: BOOL, Literal: lit, Line: startLine, Column: startCol}
	}

	return Token{Type: IDENT, Literal: lit, Line: startLine, Column: startCol}
}

func (l *NeoLexer) readString(quote byte) Token {
	startLine, startCol := l.line, l.col
	l.advance()
	startPos := l.pos
	l.litBuf = l.litBuf[:0]
	for l.peek() != quote && l.peek() != 0 {
		ch := l.advance()
		if l.isStreaming {
			l.litBuf = append(l.litBuf, ch)
		}
	}
	l.advance()
	return Token{Type: STRING, Literal: l.getLiteral(startPos, l.pos-1), Line: startLine, Column: startCol}
}

func (l *NeoLexer) readRawString() Token {
	startLine, startCol := l.line, l.col
	l.advance()
	startPos := l.pos
	l.litBuf = l.litBuf[:0]
	for l.peek() != '`' && l.peek() != 0 {
		ch := l.advance()
		if l.isStreaming {
			l.litBuf = append(l.litBuf, ch)
		}
	}
	l.advance()
	return Token{Type: STRING, Literal: l.getLiteral(startPos, l.pos-1), Line: startLine, Column: startCol}
}

func (l *NeoLexer) readNumberOrDuration() Token {
	startLine, startCol := l.line, l.col
	startPos := l.pos
	l.litBuf = l.litBuf[:0]
	for {
		ch := l.peek()
		if (ch >= '0' && ch <= '9') || ch == '.' || ch == '-' || (ch >= 'a' && ch <= 'z') || ch == 'µ' {
			ch := l.advance()
			if l.isStreaming {
				l.litBuf = append(l.litBuf, ch)
			}
		} else {
			break
		}
	}
	lit := l.getLiteral(startPos, l.pos)
	return Token{Type: INT, Literal: lit, Line: startLine, Column: startCol}
}
