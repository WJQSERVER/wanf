// wanf/stream_decoder.go

package wanf

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"reflect"
	"strconv"
	"time"
)

// StreamDecoder ... (struct definition is unchanged)
type StreamDecoder struct {
	d *internalDecoder
	p *Parser
}

// NewStreamDecoder ... (function is unchanged)
func NewStreamDecoder(r io.Reader, opts ...DecoderOption) (*StreamDecoder, error) {
	d := &internalDecoder{vars: make(map[string]interface{})}
	for _, opt := range opts {
		opt(d)
	}

	l := newStreamLexer(r)
	p := NewParser(l)

	dec := &StreamDecoder{
		d: d,
		p: p,
	}

	return dec, nil
}

// Decode ... (function is unchanged)
func (dec *StreamDecoder) Decode(v interface{}) error {
	rv := reflect.ValueOf(v)
	if rv.Kind() != reflect.Ptr || rv.Elem().Kind() != reflect.Struct {
		return fmt.Errorf("v must be a pointer to a struct")
	}

	// 修正：确保在开始解码前有 token
	if dec.p.curTokenIs(EOF) {
		// 如果文件为空，则不进行任何操作
		return nil
	}

	err := dec.decodeBody(rv.Elem())
	if err != nil && err != io.EOF {
		return err
	}
	return nil
}


// decodeBody ... (function is unchanged)
func (dec *StreamDecoder) decodeBody(rv reflect.Value) error {
	for {
		// 修正: 将 EOF 检查移到循环顶部，这是更标准的做法
		if dec.p.curTokenIs(EOF) {
			return io.EOF
		}

		switch dec.p.curToken.Type {
		case SEMICOLON, COMMENT:
			dec.p.nextToken()
			continue
		case VAR, IMPORT:
			return fmt.Errorf("wanf: var/import statements are not supported in stream decoding (line %d)", dec.p.curToken.Line)
		case IDENT:
			if dec.p.peekTokenIs(ASSIGN) {
				if err := dec.decodeAssignStatement(rv); err != nil {
					return err
				}
			} else if dec.p.peekTokenIs(LBRACE) || dec.p.peekTokenIs(STRING) {
				if err := dec.decodeBlockStatement(rv); err != nil {
					return err
				}
			} else {
				return fmt.Errorf("wanf: unexpected token %s after identifier %q on line %d", dec.p.peekToken.Type, dec.p.curToken.Literal, dec.p.curToken.Line)
			}
		case RBRACE:
			return nil // Correctly exit when a block is closed
		default:
			return fmt.Errorf("wanf: unexpected token %s at top level on line %d", dec.p.curToken.Type, dec.p.curToken.Line)
		}

		// 修正: 不再在这里调用 nextToken()，因为每个 decode* 函数现在都负责消费自己的 token
		// dec.p.nextToken()
	}
}

// decodeAssignStatement now consumes the final token of the expression
func (dec *StreamDecoder) decodeAssignStatement(rv reflect.Value) error {
	ident := dec.p.curToken

	dec.p.nextToken() // consume IDENT
	dec.p.nextToken() // consume ASSIGN

	val, err := dec.evalExpressionOnTheFly()
	if err != nil {
		return err
	}

	// 修正: evalExpressionOnTheFly 现在会消费表达式的最后一个 token
	// 所以我们不需要在这里调用 nextToken()

	field, tag, ok := findFieldAndTag(rv, ident.Literal)
	if !ok {
		// If field not found, we still need to consume tokens until the next statement
		// But evalExpressionOnTheFly already does this, so we just return.
		return nil
	}

	if tag.KeyField != "" {
		return dec.d.setMapFromList(field, val, tag.KeyField)
	}
	return dec.d.setField(field, val)
}

// decodeBlockStatement now consumes the final '}'
func (dec *StreamDecoder) decodeBlockStatement(rv reflect.Value) error {
	blockName := dec.p.curToken.Literal
	dec.p.nextToken() // consume block name

	var label string
	if dec.p.curTokenIs(STRING) {
		label = BytesToString(dec.p.curToken.Literal)
		dec.p.nextToken() // consume label
	}

	if !dec.p.curTokenIs(LBRACE) {
		return fmt.Errorf("wanf: expected '{' after block identifier on line %d", dec.p.curToken.Line)
	}
	dec.p.nextToken() // consume '{'

	field, _, ok := findFieldAndTag(rv, blockName)
	if !ok {
		return dec.skipBlock()
	}

	switch field.Kind() {
	case reflect.Struct:
		if err := dec.decodeBody(field); err != nil && err != io.EOF {
			return err
		}
	case reflect.Map:
		if field.IsNil() {
			field.Set(reflect.MakeMap(field.Type()))
		}
		mapElemType := field.Type().Elem()
		newElem := reflect.New(mapElemType).Elem()
		if err := dec.decodeBody(newElem); err != nil && err != io.EOF {
			return err
		}
		if label == "" {
			return fmt.Errorf("wanf: map block %q requires a label", BytesToString(blockName))
		}
		field.SetMapIndex(reflect.ValueOf(label), newElem)

	default:
		return fmt.Errorf("wanf: block %q cannot be decoded into field of type %s", BytesToString(blockName), field.Type())
	}

	if !dec.p.curTokenIs(RBRACE) {
		return fmt.Errorf("wanf: expected '}' to close block %q on line %d", BytesToString(blockName), dec.p.curToken.Line)
	}
	dec.p.nextToken() // *** 修正: 消费 '}' ***
	return nil
}


// evalExpressionOnTheFly now consumes the last token of the evaluated expression.
func (dec *StreamDecoder) evalExpressionOnTheFly() (interface{}, error) {
	var val interface{}
	var err error

	switch dec.p.curToken.Type {
	case INT:
		val, err = strconv.ParseInt(BytesToString(dec.p.curToken.Literal), 0, 64)
	case FLOAT:
		val, err = strconv.ParseFloat(BytesToString(dec.p.curToken.Literal), 64)
	case STRING:
		val = BytesToString(dec.p.curToken.Literal)
	case BOOL:
		val, err = strconv.ParseBool(BytesToString(dec.p.curToken.Literal))
	case DUR:
		val, err = time.ParseDuration(BytesToString(dec.p.curToken.Literal))
	case IDENT:
		if bytes.Equal(dec.p.curToken.Literal, []byte("env")) {
			return dec.evalEnvExpressionOnTheFly()
		}
		// In stream mode, other identifiers are not valid expressions
		err = fmt.Errorf("wanf: unexpected identifier %q in expression", dec.p.curToken.Literal)
	case LBRACK:
		return dec.decodeListLiteralOnTheFly()
	case LBRACE:
		if dec.p.peekTokenIs(LBRACK) {
			return dec.decodeMapLiteralOnTheFly()
		}
		return dec.decodeBlockLiteralOnTheFly()
	default:
		err = fmt.Errorf("wanf: unexpected token %s in expression", dec.p.curToken.Type)
	}

	if err != nil {
		return nil, err
	}
	// *** 核心修正: 对于简单的字面量，在返回前消费掉它的 token ***
	dec.p.nextToken()
	return val, nil
}


func (dec *StreamDecoder) decodeListLiteralOnTheFly() (interface{}, error) {
	var list []interface{}
	dec.p.nextToken() // consume '['

	for !dec.p.curTokenIs(RBRACK) && !dec.p.curTokenIs(EOF) {
		val, err := dec.evalExpressionOnTheFly()
		if err != nil {
			return nil, err
		}
		list = append(list, val)
		// evalExpressionOnTheFly already called nextToken, so curToken is now after the value.

		if dec.p.curTokenIs(COMMA) {
			dec.p.nextToken() // consume comma
		} else if !dec.p.curTokenIs(RBRACK) {
			return nil, fmt.Errorf("wanf: expected comma or ']' in list literal")
		}
	}

	if !dec.p.curTokenIs(RBRACK) {
		return nil, fmt.Errorf("wanf: unclosed list literal")
	}
	dec.p.nextToken() // *** 修正: 消费 ']' ***
	return list, nil
}

func (dec *StreamDecoder) decodeBlockLiteralOnTheFly() (interface{}, error) {
	m := make(map[string]interface{})
	dec.p.nextToken() // consume '{'

	for !dec.p.curTokenIs(RBRACE) && !dec.p.curTokenIs(EOF) {
		if dec.p.curTokenIs(COMMENT) || dec.p.curTokenIs(SEMICOLON) {
			dec.p.nextToken()
			continue
		}
		if !dec.p.curTokenIs(IDENT) {
			return nil, fmt.Errorf("wanf: expected identifier as key in block literal")
		}
		key := BytesToString(dec.p.curToken.Literal)

		dec.p.nextToken() // consume IDENT
		if !dec.p.curTokenIs(ASSIGN) {
			return nil, fmt.Errorf("wanf: expected '=' after key in block literal")
		}
		dec.p.nextToken() // consume ASSIGN

		val, err := dec.evalExpressionOnTheFly()
		if err != nil {
			return nil, err
		}
		m[key] = val
		// No nextToken here, evalExpressionOnTheFly did it.
	}
	if !dec.p.curTokenIs(RBRACE) {
		return nil, fmt.Errorf("wanf: unclosed block literal")
	}
	dec.p.nextToken() // *** 修正: 消费 '}' ***
	return m, nil
}

func (dec *StreamDecoder) decodeMapLiteralOnTheFly() (interface{}, error) {
	m := make(map[string]interface{})
	dec.p.nextToken() // consume '{'
	dec.p.nextToken() // consume '['

	for !dec.p.curTokenIs(RBRACK) && !dec.p.curTokenIs(EOF) {
		if !dec.p.curTokenIs(IDENT) {
			return nil, fmt.Errorf("wanf: expected identifier as key in map literal")
		}
		key := BytesToString(dec.p.curToken.Literal)
		dec.p.nextToken() // consume IDENT
		if !dec.p.curTokenIs(ASSIGN) {
			return nil, fmt.Errorf("wanf: expected '=' after key in map literal")
		}
		dec.p.nextToken() // consume ASSIGN

		val, err := dec.evalExpressionOnTheFly()
		if err != nil {
			return nil, err
		}
		m[key] = val

		if dec.p.curTokenIs(COMMA) {
			dec.p.nextToken()
		} else if !dec.p.curTokenIs(RBRACK) {
			return nil, fmt.Errorf("wanf: expected comma or ']' in map literal")
		}
	}
	if !dec.p.curTokenIs(RBRACK) {
		return nil, fmt.Errorf("wanf: unclosed map literal")
	}
	dec.p.nextToken() // consume ']'
	if !dec.p.curTokenIs(RBRACE) {
		return nil, fmt.Errorf("wanf: expected '}' to close map literal")
	}
	dec.p.nextToken() // *** 修正: 消费 '}' ***
	return m, nil
}

func (dec *StreamDecoder) evalEnvExpressionOnTheFly() (interface{}, error) {
	dec.p.nextToken() // consume 'env'
	if !dec.p.curTokenIs(LPAREN) {
		return nil, fmt.Errorf("wanf: expected '(' after env")
	}
	dec.p.nextToken() // consume '('

	if !dec.p.curTokenIs(STRING) {
		return nil, fmt.Errorf("wanf: expected string argument for env()")
	}
	envVarName := BytesToString(dec.p.curToken.Literal)
	dec.p.nextToken() // consume env var name string

	var val string
	var found bool

	// Check for default value
	if dec.p.curTokenIs(COMMA) {
		dec.p.nextToken() // consume ','
		if !dec.p.curTokenIs(STRING) {
			return nil, fmt.Errorf("wanf: expected string for env() default value")
		}
		defaultValue := BytesToString(dec.p.curToken.Literal)
		dec.p.nextToken() // consume default value string

		val, found = os.LookupEnv(envVarName)
		if !found {
			val = defaultValue
		}
	} else {
		// No default value
		val, found = os.LookupEnv(envVarName)
		if !found {
			return nil, fmt.Errorf("wanf: environment variable %q not set and no default provided", envVarName)
		}
	}

	if !dec.p.curTokenIs(RPAREN) {
		return nil, fmt.Errorf("wanf: expected ')' to close env() call")
	}
	dec.p.nextToken() // *** 修正: 消费 ')' ***

	return val, nil
}

// skipBlock is now much simpler because decodeBody handles block endings
func (dec *StreamDecoder) skipBlock() error {
    // We are just after the '{'. Call decodeBody with a dummy value.
    // decodeBody will consume until it sees the matching '}'
    dummyVal := reflect.ValueOf(struct{}{})
    err := dec.decodeBody(dummyVal)
    if err != nil && err != io.EOF {
        return err
    }
    // After decodeBody, the current token should be '}'
    if !dec.p.curTokenIs(RBRACE) {
        return fmt.Errorf("wanf: unclosed block while skipping")
    }
    dec.p.nextToken() // Consume the final '}'
    return nil
}
