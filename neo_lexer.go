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
	if !l.isStreaming {
		return l.nextTokenFast()
	}
	return l.nextTokenStreaming()
}

func (l *NeoLexer) nextTokenFast() Token {
	input := l.input
	read := l.read
	pos := l.pos
	line := l.line
	col := l.col

skip:
	for pos < read {
		ch := input[pos]
		switch ch {
		case ' ', '\t', '\r':
			pos++
			col++
		case '\n':
			pos++
			line++
			col = 1
		case '/':
			if pos+1 < read {
				if input[pos+1] == '/' {
					pos += 2
					for pos < read && input[pos] != '\n' { pos++ }
					continue
				} else if input[pos+1] == '*' {
					pos += 2
					for pos < read {
						if input[pos] == '*' && pos+1 < read && input[pos+1] == '/' {
							pos += 2
							col += 2
							continue skip
						}
						if input[pos] == '\n' {
							line++
							col = 1
						} else {
							col++
						}
						pos++
					}
				}
			}
			break skip
		default:
			break skip
		}
	}

	if pos >= read {
		l.pos, l.line, l.col = pos, line, col
		return Token{Type: EOF, Line: line, Column: col}
	}

	ch := input[pos]
	startLine, startCol := line, col
	pos++
	col++

	switch ch {
	case '=':
		l.pos, l.line, l.col = pos, line, col
		return Token{Type: ASSIGN, Literal: assignLit, Line: startLine, Column: startCol}
	case '{':
		l.pos, l.line, l.col = pos, line, col
		return Token{Type: LBRACE, Literal: lbraceLit, Line: startLine, Column: startCol}
	case '}':
		l.pos, l.line, l.col = pos, line, col
		return Token{Type: RBRACE, Literal: rbraceLit, Line: startLine, Column: startCol}
	case '[':
		l.pos, l.line, l.col = pos, line, col
		return Token{Type: LBRACK, Literal: lbrackLit, Line: startLine, Column: startCol}
	case ']':
		l.pos, l.line, l.col = pos, line, col
		return Token{Type: RBRACK, Literal: rbrackLit, Line: startLine, Column: startCol}
	case ',':
		l.pos, l.line, l.col = pos, line, col
		return Token{Type: COMMA, Literal: commaLit, Line: startLine, Column: startCol}
	case ';':
		l.pos, l.line, l.col = pos, line, col
		return Token{Type: SEMICOLON, Literal: semicolonLit, Line: startLine, Column: startCol}
	case '"', '\'':
		startData := pos
		for pos < read && input[pos] != ch { pos++ }
		lit := input[startData:pos]
		if pos < read { pos++ }
		col += (pos - startData)
		l.pos, l.line, l.col = pos, line, col
		return Token{Type: STRING, Literal: lit, Line: startLine, Column: startCol}
	}

	if (ch >= '0' && ch <= '9') || ch == '-' || ch == '.' {
		sp := pos - 1
		hasLetter := false
		for pos < read {
			c := input[pos]
			if (c >= '0' && c <= '9') || c == '.' || c == '-' {
				pos++
			} else if (c >= 'a' && c <= 'z') || c == 0xC2 || c == 0xB5 {
				pos++
				hasLetter = true
			} else {
				break
			}
		}
		col += (pos - sp - 1)
		l.pos, l.line, l.col = pos, line, col
		typ := INT
		if hasLetter { typ = DUR }
		return Token{Type: typ, Literal: input[sp:pos], Line: startLine, Column: startCol}
	}

	if isIdentTable[ch] {
		sp := pos - 1
		for pos < read && isIdentTable[input[pos]] { pos++ }
		col += (pos - sp - 1)
		l.pos, l.line, l.col = pos, line, col
		lit := input[sp:pos]
		if len(lit) == 6 && BytesToString(lit) == "import" { return Token{Type: IMPORT, Literal: importLit, Line: startLine, Column: startCol} }
		if len(lit) == 3 && BytesToString(lit) == "var" { return Token{Type: VAR, Literal: varLit, Line: startLine, Column: startCol} }
		return Token{Type: IDENT, Literal: lit, Line: startLine, Column: startCol}
	}

	l.pos, l.line, l.col = pos, line, col
	return Token{Type: ILLEGAL, Literal: input[pos-1 : pos], Line: startLine, Column: startCol}
}

func (l *NeoLexer) nextTokenStreaming() Token {
	l.skipWhitespace(); ch := l.peek()
	if ch == 0 { return Token{Type: EOF, Line: l.line, Column: l.col} }
	startLine, startCol := l.line, l.col
	switch ch {
	case '=': l.advance(); return Token{Type: ASSIGN, Literal: assignLit, Line: startLine, Column: startCol}
	case '{': l.advance(); return Token{Type: LBRACE, Literal: lbraceLit, Line: startLine, Column: startCol}
	case '}': l.advance(); return Token{Type: RBRACE, Literal: rbraceLit, Line: startLine, Column: startCol}
	case '[': l.advance(); return Token{Type: LBRACK, Literal: lbrackLit, Line: startLine, Column: startCol}
	case ']': l.advance(); return Token{Type: RBRACK, Literal: rbrackLit, Line: startLine, Column: startCol}
	case ',': l.advance(); return Token{Type: COMMA, Literal: commaLit, Line: startLine, Column: startCol}
	case ';': l.advance(); return Token{Type: SEMICOLON, Literal: semicolonLit, Line: startLine, Column: startCol}
	}
	if (ch >= '0' && ch <= '9') || ch == '-' || ch == '.' { return l.readNumberStreaming() }
	if isIdentTable[ch] { return l.readIdentifierStreaming() }
	if ch == '"' || ch == '\'' { return l.readStringStreaming(ch) }
	if ch == '`' { return l.readRawStringStreaming() }
	l.advance()
	return Token{Type: ILLEGAL, Literal: []byte{ch}, Line: startLine, Column: startCol}
}

func (l *NeoLexer) peek() byte {
	if l.pos >= l.read {
		if l.reader != nil {
			n, err := l.reader.Read(l.buf)
			if n > 0 { l.input = l.buf; l.read = n; l.pos = 0 }
			if err != nil { l.err = err; if n == 0 { return 0 } }
		} else { return 0 }
	}
	return l.input[l.pos]
}

func (l *NeoLexer) advance() byte {
	ch := l.peek()
	if ch != 0 {
		l.pos++
		if ch == '\n' { l.line++; l.col = 1 } else { l.col++ }
	}
	return ch
}

func (l *NeoLexer) skipWhitespace() {
	for {
		ch := l.peek()
		if ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' { l.advance() } else if ch == '/' {
			l.advance()
			next := l.peek()
			if next == '/' {
				l.advance()
				for l.peek() != '\n' && l.peek() != 0 { l.advance() }
			} else if next == '*' {
				l.advance()
				for {
					c := l.advance()
					if c == '*' && l.peek() == '/' { l.advance(); break }
					if c == 0 { break }
				}
			} else {
				break
			}
		} else { break }
	}
}

func (l *NeoLexer) readIdentifierStreaming() Token {
	startLine, startCol := l.line, l.col; l.litBuf = l.litBuf[:0]
	for isIdentTable[l.peek()] { l.litBuf = append(l.litBuf, l.advance()) }
	if len(l.litBuf) == 6 && BytesToString(l.litBuf) == "import" { return Token{Type: IMPORT, Literal: importLit, Line: startLine, Column: startCol} }
	if len(l.litBuf) == 3 && BytesToString(l.litBuf) == "var" { return Token{Type: VAR, Literal: varLit, Line: startLine, Column: startCol} }
	return Token{Type: IDENT, Literal: l.litBuf, Line: startLine, Column: startCol}
}

func (l *NeoLexer) readStringStreaming(quote byte) Token {
	startLine, startCol := l.line, l.col; l.advance(); l.litBuf = l.litBuf[:0]
	for l.peek() != quote && l.peek() != 0 { l.litBuf = append(l.litBuf, l.advance()) }
	l.advance(); return Token{Type: STRING, Literal: l.litBuf, Line: startLine, Column: startCol}
}

func (l *NeoLexer) readRawStringStreaming() Token {
	startLine, startCol := l.line, l.col; l.advance(); l.litBuf = l.litBuf[:0]
	for l.peek() != '`' && l.peek() != 0 { l.litBuf = append(l.litBuf, l.advance()) }
	l.advance(); return Token{Type: STRING, Literal: l.litBuf, Line: startLine, Column: startCol}
}

func (l *NeoLexer) readNumberStreaming() Token {
	startLine, startCol := l.line, l.col; l.litBuf = l.litBuf[:0]
	hasLetter := false
	for {
		ch := l.peek()
		if (ch >= '0' && ch <= '9') || ch == '.' || ch == '-' {
			l.litBuf = append(l.litBuf, l.advance())
		} else if (ch >= 'a' && ch <= 'z') || ch == 0xC2 || ch == 0xB5 {
			l.litBuf = append(l.litBuf, l.advance())
			hasLetter = true
		} else { break }
	}
	typ := INT
	if hasLetter { typ = DUR }
	return Token{Type: typ, Literal: l.litBuf, Line: startLine, Column: startCol}
}
