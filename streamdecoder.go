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

// StreamDecoder 结构体用于从输入流中解码WANF格式数据。
type StreamDecoder struct {
	d *internalDecoder // 内部解码器
	p *Parser          // 语法解析器
}

// NewStreamDecoder 创建并返回一个新的StreamDecoder实例。
// 它接收一个io.Reader作为输入源，以及可选的DecoderOption配置选项。
func NewStreamDecoder(r io.Reader, opts ...DecoderOption) (*StreamDecoder, error) {
	d := &internalDecoder{vars: make(map[string]interface{})}
	for _, opt := range opts {
		opt(d)
	}

	l := newStreamLexer(r) // 创建流式词法分析器
	p := NewParser(l)      // 创建语法解析器

	dec := &StreamDecoder{
		d: d,
		p: p,
	}

	return dec, nil
}

// Decode 将输入流中的WANF数据解码到v指向的结构体中。
// v必须是一个指向结构体的指针。
func (dec *StreamDecoder) Decode(v interface{}) error {
	rv := reflect.ValueOf(v)
	// 验证v是否为结构体指针
	if rv.Kind() != reflect.Ptr || rv.Elem().Kind() != reflect.Struct {
		return fmt.Errorf("v must be a pointer to a struct")
	}

	// 确保在开始解码前有可用的token
	if dec.p.curTokenIs(EOF) {
		// 如果输入为空文件，则直接返回，不进行任何操作
		return nil
	}

	// 解码结构体主体内容
	err := dec.decodeBody(rv.Elem())
	if err != nil && err != io.EOF {
		return err
	}
	return nil
}

// decodeBody 解码结构体的主体内容，处理顶层语句和块。
func (dec *StreamDecoder) decodeBody(rv reflect.Value) error {
	for {
		// 将EOF检查放在循环顶部是更标准的做法，用于判断是否到达文件末尾
		if dec.p.curTokenIs(EOF) {
			return io.EOF
		}

		switch dec.p.curToken.Type {
		case SEMICOLON, COMMENT: // 忽略分号和注释
			dec.p.nextToken()
			continue
		case VAR, IMPORT: // 流式解码不支持var/import语句
			return fmt.Errorf("wanf: var/import statements are not supported in stream decoding (line %d)", dec.p.curToken.Line)
		case IDENT: // 处理标识符
			if dec.p.peekTokenIs(ASSIGN) { // 如果是赋值语句
				if err := dec.decodeAssignStatement(rv); err != nil {
					return err
				}
			} else if dec.p.peekTokenIs(LBRACE) || dec.p.peekTokenIs(STRING) { // 如果是块语句（带或不带标签）
				if err := dec.decodeBlockStatement(rv); err != nil {
					return err
				}
			} else { // 标识符后出现意外token
				return fmt.Errorf("wanf: unexpected token %s after identifier %q on line %d", dec.p.peekToken.Type, dec.p.curToken.Literal, dec.p.curToken.Line)
			}
		case RBRACE: // 遇到右花括号，表示当前块结束
			return nil
		default: // 顶层出现意外token
			return fmt.Errorf("wanf: unexpected token %s at top level on line %d", dec.p.curToken.Type, dec.p.curToken.Line)
		}

		// 不再在此处调用nextToken()，因为每个decode*函数现在都负责消费其自身的token
	}
}

// decodeAssignStatement 解码赋值语句。
func (dec *StreamDecoder) decodeAssignStatement(rv reflect.Value) error {
	// 在所有nextToken()调用之前复制标识符名称
	identName := string(dec.p.curToken.Literal)

	dec.p.nextToken()              // 消费标识符
	if !dec.p.curTokenIs(ASSIGN) { // 更安全的检查方式：确保当前token是赋值符号
		return fmt.Errorf("wanf: expected '=' after identifier %q", identName)
	}
	dec.p.nextToken() // 消费赋值符号

	// 实时评估表达式的值
	val, err := dec.evalExpressionOnTheFly()
	if err != nil {
		return err
	}

	// 查找结构体字段及其标签，使用identName的安全副本
	field, tag, ok := findFieldAndTag(rv, []byte(identName))
	if !ok {
		// 如果未找到字段，仍然需要消费token直到下一个语句
		// 但evalExpressionOnTheFly已经完成了这一步，所以直接返回
		return nil
	}

	if tag.KeyField != "" { // 如果字段是map且指定了key字段
		return dec.d.setMapFromList(field, val, tag.KeyField)
	}
	return dec.d.setField(field, val) // 设置结构体字段值
}

// decodeBlockStatement 解码块语句，现在负责消费末尾的'}'。
func (dec *StreamDecoder) decodeBlockStatement(rv reflect.Value) error {
	blockName := dec.p.curToken.Literal // 块标识符名称
	dec.p.nextToken()                   // 消费块名称

	var label string
	if dec.p.curTokenIs(STRING) { // 如果块带有字符串标签
		label = BytesToString(dec.p.curToken.Literal)
		dec.p.nextToken() // 消费标签
	}

	if !dec.p.curTokenIs(LBRACE) { // 期望块标识符后跟'{'
		return fmt.Errorf("wanf: expected '{' after block identifier on line %d", dec.p.curToken.Line)
	}
	dec.p.nextToken() // 消费'{'

	// 查找结构体字段及其标签
	field, _, ok := findFieldAndTag(rv, blockName)
	if !ok {
		return dec.skipBlock() // 如果未找到字段，则跳过整个块
	}

	switch field.Kind() {
	case reflect.Struct: // 如果字段是结构体类型
		// 递归解码结构体内部内容
		if err := dec.decodeBody(field); err != nil && err != io.EOF {
			return err
		}
	case reflect.Map: // 如果字段是map类型
		if field.IsNil() {
			field.Set(reflect.MakeMap(field.Type())) // 如果map为空，则初始化
		}
		mapElemType := field.Type().Elem()         // 获取map元素类型
		newElem := reflect.New(mapElemType).Elem() // 创建新的map元素实例
		// 解码map元素内部内容
		if err := dec.decodeBody(newElem); err != nil && err != io.EOF {
			return err
		}
		if label == "" { // map块需要一个标签作为key
			return fmt.Errorf("wanf: map block %q requires a label", BytesToString(blockName))
		}
		field.SetMapIndex(reflect.ValueOf(label), newElem) // 将解码后的元素设置到map中
	default: // 不支持的块解码类型
		return fmt.Errorf("wanf: block %q cannot be decoded into field of type %s", BytesToString(blockName), field.Type())
	}

	if !dec.p.curTokenIs(RBRACE) { // 期望块以'}'结束
		return fmt.Errorf("wanf: expected '}' to close block %q on line %d", BytesToString(blockName), dec.p.curToken.Line)
	}
	dec.p.nextToken() // 消费'}'
	return nil
}

// evalExpressionOnTheFly 实时评估表达式的值，并消费表达式的最后一个token。
func (dec *StreamDecoder) evalExpressionOnTheFly() (interface{}, error) {
	var val interface{}
	var err error

	switch dec.p.curToken.Type {
	case INT: // 整数
		val, err = strconv.ParseInt(BytesToString(dec.p.curToken.Literal), 0, 64)
	case FLOAT: // 浮点数
		val, err = strconv.ParseFloat(BytesToString(dec.p.curToken.Literal), 64)
	case STRING: // 字符串
		val = BytesToString(dec.p.curToken.Literal)
	case BOOL: // 布尔值
		val, err = strconv.ParseBool(BytesToString(dec.p.curToken.Literal))
	case DUR: // 时间段 (duration)
		val, err = time.ParseDuration(BytesToString(dec.p.curToken.Literal))
	case IDENT: // 标识符
		if bytes.Equal(dec.p.curToken.Literal, []byte("env")) { // 如果是env()函数调用
			return dec.evalEnvExpressionOnTheFly()
		}
		// 在流模式下，其他标识符不是有效的表达式
		err = fmt.Errorf("wanf: unexpected identifier %q in expression", dec.p.curToken.Literal)
	case LBRACK: // 列表字面量
		return dec.decodeListLiteralOnTheFly()
	case LBRACE: // 块字面量或map字面量
		if dec.p.peekTokenIs(LBRACK) { // 如果是 { [ ... ] } 形式的map字面量
			return dec.decodeMapLiteralOnTheFly()
		}
		return dec.decodeBlockLiteralOnTheFly() // 普通块字面量
	default: // 表达式中出现意外token
		err = fmt.Errorf("wanf: unexpected token %s in expression", dec.p.curToken.Type)
	}

	if err != nil {
		return nil, err
	}
	// 核心修正: 对于简单的字面量，在返回前消费掉其token
	dec.p.nextToken()
	return val, nil
}

// decodeListLiteralOnTheFly 实时解码列表字面量，并消费末尾的']'。
func (dec *StreamDecoder) decodeListLiteralOnTheFly() (interface{}, error) {
	var list []interface{}
	dec.p.nextToken() // 消费'['

	for !dec.p.curTokenIs(RBRACK) && !dec.p.curTokenIs(EOF) {
		val, err := dec.evalExpressionOnTheFly() // 评估列表元素
		if err != nil {
			return nil, err
		}
		list = append(list, val)
		// evalExpressionOnTheFly已经调用了nextToken，所以curToken现在位于值之后。

		if dec.p.curTokenIs(COMMA) { // 如果有逗号
			dec.p.nextToken() // 消费逗号
		} else if !dec.p.curTokenIs(RBRACK) { // 期望逗号或']'
			return nil, fmt.Errorf("wanf: expected comma or ']' in list literal")
		}
	}

	if !dec.p.curTokenIs(RBRACK) { // 列表未闭合
		return nil, fmt.Errorf("wanf: unclosed list literal")
	}
	dec.p.nextToken() // 消费']'
	return list, nil
}

// decodeBlockLiteralOnTheFly 实时解码块字面量（map[string]interface{}），并消费末尾的'}'。
func (dec *StreamDecoder) decodeBlockLiteralOnTheFly() (interface{}, error) {
	m := make(map[string]interface{})
	dec.p.nextToken() // 消费'{'

	for !dec.p.curTokenIs(RBRACE) && !dec.p.curTokenIs(EOF) {
		if dec.p.curTokenIs(COMMENT) || dec.p.curTokenIs(SEMICOLON) { // 忽略注释和分号
			dec.p.nextToken()
			continue
		}
		if !dec.p.curTokenIs(IDENT) { // 期望标识符作为块字面量的键
			return nil, fmt.Errorf("wanf: expected identifier as key in block literal")
		}
		key := BytesToString(dec.p.curToken.Literal) // 获取键

		dec.p.nextToken()              // 消费标识符
		if !dec.p.curTokenIs(ASSIGN) { // 期望键后跟'='
			return nil, fmt.Errorf("wanf: expected '=' after key in block literal")
		}
		dec.p.nextToken() // 消费赋值符号

		val, err := dec.evalExpressionOnTheFly() // 评估值
		if err != nil {
			return nil, err
		}
		m[key] = val // 设置map键值对
		// 此处不需要nextToken，因为evalExpressionOnTheFly已经处理了。
	}
	if !dec.p.curTokenIs(RBRACE) { // 块字面量未闭合
		return nil, fmt.Errorf("wanf: unclosed block literal")
	}
	dec.p.nextToken() // 消费'}'
	return m, nil
}

// decodeMapLiteralOnTheFly 实时解码map字面量（{ [key=value, ...] } 形式），并消费末尾的'}'。
func (dec *StreamDecoder) decodeMapLiteralOnTheFly() (interface{}, error) {
	m := make(map[string]interface{})
	dec.p.nextToken() // 消费'{'
	dec.p.nextToken() // 消费'['

	for !dec.p.curTokenIs(RBRACK) && !dec.p.curTokenIs(EOF) {
		if !dec.p.curTokenIs(IDENT) { // 期望标识符作为map字面量的键
			return nil, fmt.Errorf("wanf: expected identifier as key in map literal")
		}
		key := BytesToString(dec.p.curToken.Literal) // 获取键
		dec.p.nextToken()                            // 消费标识符
		if !dec.p.curTokenIs(ASSIGN) {               // 期望键后跟'='
			return nil, fmt.Errorf("wanf: expected '=' after key in map literal")
		}
		dec.p.nextToken() // 消费赋值符号

		val, err := dec.evalExpressionOnTheFly() // 评估值
		if err != nil {
			return nil, err
		}
		m[key] = val // 设置map键值对

		if dec.p.curTokenIs(COMMA) { // 如果有逗号
			dec.p.nextToken() // 消费逗号
		} else if !dec.p.curTokenIs(RBRACK) { // 期望逗号或']'
			return nil, fmt.Errorf("wanf: expected comma or ']' in map literal")
		}
	}
	if !dec.p.curTokenIs(RBRACK) { // map内部列表未闭合
		return nil, fmt.Errorf("wanf: unclosed map literal")
	}
	dec.p.nextToken()              // 消费']'
	if !dec.p.curTokenIs(RBRACE) { // map字面量未闭合
		return nil, fmt.Errorf("wanf: expected '}' to close map literal")
	}
	dec.p.nextToken() // 消费'}'
	return m, nil
}

// evalEnvExpressionOnTheFly 实时评估env()函数表达式，并消费末尾的')'。
func (dec *StreamDecoder) evalEnvExpressionOnTheFly() (interface{}, error) {
	dec.p.nextToken()              // 消费'env'
	if !dec.p.curTokenIs(LPAREN) { // 期望'('
		return nil, fmt.Errorf("wanf: expected '(' after env")
	}
	dec.p.nextToken() // 消费'('

	if !dec.p.curTokenIs(STRING) { // 期望env()的参数是字符串
		return nil, fmt.Errorf("wanf: expected string argument for env()")
	}
	envVarName := BytesToString(dec.p.curToken.Literal) // 获取环境变量名
	dec.p.nextToken()                                   // 消费环境变量名字符串

	var val string
	var found bool

	// 检查是否有默认值
	if dec.p.curTokenIs(COMMA) {
		dec.p.nextToken()              // 消费','
		if !dec.p.curTokenIs(STRING) { // 期望默认值是字符串
			return nil, fmt.Errorf("wanf: expected string for env() default value")
		}
		defaultValue := BytesToString(dec.p.curToken.Literal) // 获取默认值
		dec.p.nextToken()                                     // 消费默认值字符串

		val, found = os.LookupEnv(envVarName) // 查找环境变量
		if !found {
			val = defaultValue // 如果未找到，使用默认值
		}
	} else {
		// 没有默认值
		val, found = os.LookupEnv(envVarName) // 查找环境变量
		if !found {
			return nil, fmt.Errorf("wanf: environment variable %q not set and no default provided", envVarName)
		}
	}

	if !dec.p.curTokenIs(RPAREN) { // 期望')'关闭env()调用
		return nil, fmt.Errorf("wanf: expected ')' to close env() call")
	}
	dec.p.nextToken() // 消费')'

	return val, nil
}

// skipBlock 跳过当前块的内容。
// 由于decodeBody现在能够处理块的结束，此函数变得更简单。
func (dec *StreamDecoder) skipBlock() error {
	// 当前token位于'{'之后。调用decodeBody并传入一个虚拟值。
	// decodeBody将一直消费token直到遇到匹配的'}'。
	dummyVal := reflect.ValueOf(struct{}{}) // 创建一个空的reflect.Value作为占位符
	err := dec.decodeBody(dummyVal)
	if err != nil && err != io.EOF {
		return err
	}
	// decodeBody返回后，当前token应该是'}'
	if !dec.p.curTokenIs(RBRACE) {
		return fmt.Errorf("wanf: unclosed block while skipping")
	}
	dec.p.nextToken() // 消费最终的'}'
	return nil
}
