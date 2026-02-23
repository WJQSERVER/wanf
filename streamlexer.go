package wanf

import (
	"bufio"
	"bytes"
	"io"
	"sync"
)

// This file contains the stream-based lexer.

var dot = []byte{'.'}

var streamLexerPool = sync.Pool{
	New: func() any {
		return &streamLexer{
			r: bufio.NewReaderSize(bytes.NewReader([]byte{}), 4096),
		}
	},
}

// streamLexer 是一个从 io.Reader 读取数据的词法分析器.
// 它使用 bufio.Reader 来实现高效的预读(peek)功能, 并使用两个交替的 bytes.Buffer 来实现零分配的词法单元字面量生成.
type streamLexer struct {
	r       *bufio.Reader
	ch      byte
	line    int
	column  int
	bufA    bytes.Buffer
	bufB    bytes.Buffer
	useBufA bool
}

// newStreamLexer creates a new stream-based lexer.
func newStreamLexer(r io.Reader) *streamLexer {
	l := streamLexerPool.Get().(*streamLexer)
	if r == nil {
		r = bytes.NewReader([]byte{})
	}
	l.r.Reset(r)
	l.ch = 0
	l.line = 1
	l.column = 0
	l.bufA.Reset()
	l.bufB.Reset()
	l.useBufA = true

	l.readChar()
	return l
}

func putStreamLexer(l *streamLexer) {
	l.r.Reset(bytes.NewReader([]byte{}))
	streamLexerPool.Put(l)
}

func (l *streamLexer) readChar() {
	b, err := l.r.ReadByte()
	if err != nil {
		l.ch = 0
	} else {
		l.ch = b
	}
	l.column++
}

func (l *streamLexer) peekChar() byte {
	b, err := l.r.Peek(1)
	if err != nil {
		return 0
	}
	return b[0]
}

func (l *streamLexer) newToken(tokenType TokenType, ch byte, line, column int) Token {
	return Token{Type: tokenType, Literal: singleCharByteSlices[ch], Line: line, Column: column}
}

func (l *streamLexer) IsPersistent() bool {
	return false
}

func (l *streamLexer) NextToken() Token {
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
			tok = Token{Type: DOLLAR_LBRACE, Literal: tokenDollarLbraceLiteral, Line: line, Column: col}
		} else {
			tok = l.newToken(ILLEGAL, l.ch, line, col)
		}
	case '"', '\'', '`':
		quote := l.ch
		tok.Type = STRING
		tok.Literal = l.readString(quote)
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
		} else if (l.ch >= '0' && l.ch <= '9') || (l.ch == '-' && ((l.peekChar() >= '0' && l.peekChar() <= '9') || l.peekChar() == '.')) {
			literal := l.readNumber()
			if isDurationUnit(l.ch, l.peekChar()) {
				tok.Type = DUR
				tok.Literal = l.readDurationCompound(literal)
			} else {
				if bytes.Contains(literal, dot) {
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

const defaultBufferSize = 64

func (l *streamLexer) activeBuffer() *bytes.Buffer {
	var buf *bytes.Buffer
	if l.useBufA {
		buf = &l.bufA
	} else {
		buf = &l.bufB
	}
	l.useBufA = !l.useBufA
	buf.Reset()
	buf.Grow(defaultBufferSize)
	return buf
}

func (l *streamLexer) readDurationCompound(prefix []byte) []byte {
	buf := l.activeBuffer()
	buf.Write(prefix)
	for {
		l.appendDurationSuffix(buf)
		// 检查下一个部分是否为持续时间的数字
		if (l.ch >= '0' && l.ch <= '9') || l.ch == '.' {
			if l.peekNextNumberHasUnit() {
				l.appendNumber(buf)
				continue
			}
		}
		break
	}
	return buf.Bytes()
}

func (l *streamLexer) appendDurationSuffix(buf *bytes.Buffer) {
	if l.ch == 'm' || l.ch == 'u' || l.ch == 'n' {
		if l.peekChar() == 's' {
			buf.WriteByte(l.ch)
			l.readChar()
		}
	} else if l.ch == 0xC2 && l.peekChar() == 0xB5 {
		buf.WriteByte(l.ch)
		l.readChar()
		if l.peekChar() == 's' {
			buf.WriteByte(l.ch)
			l.readChar()
		}
	}
	buf.WriteByte(l.ch)
	l.readChar()
}

func (l *streamLexer) peekNextNumberHasUnit() bool {
	isFloat := false
	ch := l.ch
	if ch == '.' {
		isFloat = true
	} else if !(ch >= '0' && ch <= '9') {
		return false
	}

	// 减少 Peek 范围以提升性能, 通常持续时间的数字部分不会很长
	peek, _ := l.r.Peek(16)
	i := 0
	for i < len(peek) {
		c := peek[i]
		if c >= '0' && c <= '9' {
			i++
		} else if c == '.' && !isFloat {
			isFloat = true
			i++
		} else {
			break
		}
	}

	var unitChar byte
	var unitNext byte
	if i < len(peek) {
		unitChar = peek[i]
		if i+1 < len(peek) {
			unitNext = peek[i+1]
		}
	} else {
		return false
	}

	return isDurationUnit(unitChar, unitNext)
}

func (l *streamLexer) skipWhitespace() {
	for {
		switch l.ch {
		case ' ', '\t', '\r':
			l.column++
		case '\n':
			l.line++
			l.column = 0
		default:
			return
		}

		// 使用 ReadByte 循环代替复杂的 Peek 逻辑，通常更快
		for {
			b, err := l.r.ReadByte()
			if err != nil {
				l.ch = 0
				return
			}
			l.ch = b
			switch l.ch {
			case ' ', '\t', '\r':
				l.column++
			case '\n':
				l.line++
				l.column = 0
			default:
				return
			}
		}
	}
}

func (l *streamLexer) readSingleLineComment() []byte {
	buf := l.activeBuffer()
	for l.ch != '\n' && l.ch != 0 {
		buf.WriteByte(l.ch)
		l.readChar()
	}
	return buf.Bytes()
}

func (l *streamLexer) readMultiLineComment() ([]byte, bool) {
	buf := l.activeBuffer()
	startLine, startCol := l.line, l.column
	buf.WriteByte(l.ch)
	l.readChar()
	buf.WriteByte(l.ch)
	l.readChar()
	for {
		if l.ch == 0 {
			l.line, l.column = startLine, startCol
			return buf.Bytes(), false
		}
		if l.ch == '*' && l.peekChar() == '/' {
			buf.WriteByte(l.ch)
			l.readChar()
			buf.WriteByte(l.ch)
			l.readChar()
			break
		}
		if l.ch == '\n' {
			l.line++
			l.column = 0
		}
		buf.WriteByte(l.ch)
		l.readChar()
	}
	return buf.Bytes(), true
}

func (l *streamLexer) readIdentifier() []byte {

	buf := l.activeBuffer()
	for {
		ch := l.ch
		if ch < 128 {
			if !isIdentTable[ch] {
				break
			}
		} else {
			if !isIdentifierCharNonASCII(ch) {
				break
			}
		}
		buf.WriteByte(ch)

		// 尝试批量读取以减少 ReadByte 调用
		peek, _ := l.r.Peek(32)
		i := 0
		for i < len(peek) {
			c := peek[i]
			if c < 128 {
				if isIdentTable[c] {
					i++
					continue
				}
			} else if isIdentifierCharNonASCII(c) {
				i++
				continue
			}
			break
		}
		if i > 0 {
			buf.Write(peek[:i])
			l.r.Discard(i)
			l.column += i
			// 更新当前字符
			b, err := l.r.ReadByte()
			if err != nil {
				l.ch = 0
			} else {
				l.ch = b
			}
			l.column++
		} else {
			l.readChar()
		}
	}
	return buf.Bytes()
}

func (l *streamLexer) appendNumber(buf *bytes.Buffer) {
	isFloat := false
	if l.ch == '-' {
		buf.WriteByte(l.ch)
		l.readChar()
	}
	for {
		if (l.ch >= '0' && l.ch <= '9') || (l.ch == '.' && !isFloat) {
			if l.ch == '.' {
				isFloat = true
			}
			buf.WriteByte(l.ch)
		} else {
			break
		}

		peek, _ := l.r.Peek(32)
		i := 0
		for i < len(peek) {
			c := peek[i]
			if c >= '0' && c <= '9' {
				i++
			} else if c == '.' && !isFloat {
				isFloat = true
				i++
			} else {
				break
			}
		}
		if i > 0 {
			buf.Write(peek[:i])
			l.r.Discard(i)
			l.column += i
		}
		l.readChar()
	}
}

func (l *streamLexer) readNumber() []byte {
	buf := l.activeBuffer()
	l.appendNumber(buf)
	return buf.Bytes()
}

func (l *streamLexer) readString(quote byte) []byte {
	buf := l.activeBuffer()
	l.readChar()
	for {
		if l.ch == quote || l.ch == 0 {
			break
		}
		buf.WriteByte(l.ch)
		l.readChar()
	}
	l.readChar()
	return buf.Bytes()
}

func (l *streamLexer) readUntilEndOfLine() []byte {
	buf := l.activeBuffer()
	for l.ch != '\n' && l.ch != '\r' && l.ch != 0 {
		buf.WriteByte(l.ch)
		l.readChar()
	}
	return buf.Bytes()
}
