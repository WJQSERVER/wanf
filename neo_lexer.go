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
	importLit    = []byte("import")
	varLit       = []byte("var")
)

type NeoLexer struct {
	input []byte
	pos   int
	read  int
	line  int
	col   int

	reader io.Reader
	buf    []byte
	err    error

	litBuf []byte
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
}

func (l *NeoLexer) Close() {
	l.reader = nil
	l.input = nil
	neoLexerPool.Put(l)
}

func (l *NeoLexer) nextToken() Token {
	l.skipWhitespace()

	ch := l.peek()
	if ch == 0 {
		return Token{Type: EOF, Line: l.line, Column: l.col}
	}

	startLine, startCol := l.line, l.col

	switch ch {
	case '=':
		l.advance()
		return Token{Type: ASSIGN, Literal: assignLit, Line: startLine, Column: startCol}
	case '{':
		l.advance()
		return Token{Type: LBRACE, Literal: lbraceLit, Line: startLine, Column: startCol}
	case '}':
		l.advance()
		return Token{Type: RBRACE, Literal: rbraceLit, Line: startLine, Column: startCol}
	case '[':
		l.advance()
		return Token{Type: LBRACK, Literal: lbrackLit, Line: startLine, Column: startCol}
	case ']':
		l.advance()
		return Token{Type: RBRACK, Literal: rbrackLit, Line: startLine, Column: startCol}
	case ',':
		l.advance()
		return Token{Type: COMMA, Literal: commaLit, Line: startLine, Column: startCol}
	case ';':
		l.advance()
		return Token{Type: SEMICOLON, Literal: semicolonLit, Line: startLine, Column: startCol}
	case '"', '\'':
		return l.readString(ch)
	case '`':
		return l.readRawString()
	default:
		if isIdentTable[ch] {
			return l.readIdentifierOrKeyword()
		}
		if (ch >= '0' && ch <= '9') || ch == '-' || ch == '.' {
			return l.readNumberOrDuration()
		}
	}

	l.advance()
	return Token{Type: ILLEGAL, Literal: []byte{ch}, Line: startLine, Column: startCol}
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
		if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' {
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

func (l *NeoLexer) getLiteral(startPos int) []byte {
	if !l.isStreaming {
		return l.input[startPos:l.pos]
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

	lit := l.getLiteral(startPos)

	if len(lit) == 6 && BytesToString(lit) == "import" {
		return Token{Type: IMPORT, Literal: importLit, Line: startLine, Column: startCol}
	}
	if len(lit) == 3 && BytesToString(lit) == "var" {
		return Token{Type: VAR, Literal: varLit, Line: startLine, Column: startCol}
	}

	return Token{Type: IDENT, Literal: lit, Line: startLine, Column: startCol}
}

func (l *NeoLexer) readString(quote byte) Token {
	startLine, startCol := l.line, l.col
	l.advance() // skip quote
	startPos := l.pos
	l.litBuf = l.litBuf[:0]
	for l.peek() != quote && l.peek() != 0 {
		ch := l.advance()
		if l.isStreaming {
			l.litBuf = append(l.litBuf, ch)
		}
	}
	l.advance() // skip quote
	return Token{Type: STRING, Literal: l.getLiteral(startPos), Line: startLine, Column: startCol}
}

func (l *NeoLexer) readRawString() Token {
	startLine, startCol := l.line, l.col
	l.advance() // skip `
	startPos := l.pos
	l.litBuf = l.litBuf[:0]
	for l.peek() != '`' && l.peek() != 0 {
		ch := l.advance()
		if l.isStreaming {
			l.litBuf = append(l.litBuf, ch)
		}
	}
	l.advance() // skip `
	return Token{Type: STRING, Literal: l.getLiteral(startPos), Line: startLine, Column: startCol}
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
	lit := l.getLiteral(startPos)
	return Token{Type: INT, Literal: lit, Line: startLine, Column: startCol}
}
