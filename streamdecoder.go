// wanf/stream_decoder.go

package wanf

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"reflect"
	"strconv"
	"sync"
	"time"
)

// StreamDecoder 结构体用于从输入流中解码WANF格式数据。
type StreamDecoder struct {
	d         *internalDecoder // 内部解码器
	p         *Parser          // 语法解析器
	processed map[string]bool  // 已处理的导入路径
}

var streamDecoderPool = sync.Pool{
	New: func() any {
		return &StreamDecoder{
			d:         &internalDecoder{vars: make(map[string]any)},
			processed: make(map[string]bool),
		}
	},
}

// NewStreamDecoder 创建并返回一个新的StreamDecoder实例。
// 它接收一个io.Reader作为输入源，以及可选的DecoderOption配置选项。
func NewStreamDecoder(r io.Reader, opts ...DecoderOption) (*StreamDecoder, error) {
	l := newStreamLexer(r) // 创建流式词法分析器
	return newStreamDecoderInternal(l, opts...), nil
}

func newStreamDecoderInternal(l lexer, opts ...DecoderOption) *StreamDecoder {
	dec := streamDecoderPool.Get().(*StreamDecoder)
	// Reset internalDecoder
	dec.d.basePath = ""
	for k := range dec.d.vars {
		delete(dec.d.vars, k)
	}
	for _, opt := range opts {
		opt(dec.d)
	}

	// Reset processed map
	for k := range dec.processed {
		delete(dec.processed, k)
	}

	if dec.p == nil {
		dec.p = NewParser(l)
	} else {
		dec.p.l = l
		dec.p.errors = dec.p.errors[:0]
		dec.p.lintErrors = dec.p.lintErrors[:0]
		dec.p.nextToken()
		dec.p.nextToken()
	}

	return dec
}

func (dec *StreamDecoder) Close() {
	if sl, ok := dec.p.l.(*streamLexer); ok {
		putStreamLexer(sl)
	}
	streamDecoderPool.Put(dec)
}

func (dec *StreamDecoder) safeString(b []byte) string {
	if dec.p.l.IsPersistent() {
		return BytesToString(b)
	}
	return string(b)
}

func putStreamDecoder(dec *StreamDecoder) {
	dec.Close()
}

// Decode 将输入流中的WANF数据解码到v指向的结构体中。
// v必须是一个指向结构体的指针。
func (dec *StreamDecoder) Decode(v any) error {
	rv := reflect.ValueOf(v)
	// 验证v是否为结构体指针
	if rv.Kind() != reflect.Pointer || rv.Elem().Kind() != reflect.Struct {
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
		case VAR: // 处理变量声明
			if err := dec.decodeVarStatement(); err != nil {
				return err
			}
		case IMPORT: // 处理导入
			if err := dec.decodeImportStatement(rv); err != nil {
				return err
			}
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

func (dec *StreamDecoder) decodeVarStatement() error {
	dec.p.nextToken() // 消费 'var'
	if !dec.p.curTokenIs(IDENT) {
		return fmt.Errorf("wanf: expected identifier after 'var' on line %d", dec.p.curToken.Line)
	}
	varName := dec.safeString(dec.p.curToken.Literal)
	dec.p.nextToken()

	if !dec.p.curTokenIs(ASSIGN) {
		return fmt.Errorf("wanf: expected '=' after variable name %q", varName)
	}
	dec.p.nextToken()

	val, err := dec.evalExpressionOnTheFly()
	if err != nil {
		return err
	}
	dec.d.vars[varName] = val
	return nil
}

func (dec *StreamDecoder) decodeImportStatement(rv reflect.Value) error {
	dec.p.nextToken() // 消费 'import'
	if !dec.p.curTokenIs(STRING) {
		return fmt.Errorf("wanf: expected string after 'import' on line %d", dec.p.curToken.Line)
	}
	importPath := dec.safeString(dec.p.curToken.Literal)
	dec.p.nextToken()

	fullPath := filepath.Join(dec.d.basePath, importPath)
	absPath, err := filepath.Abs(fullPath)
	if err != nil {
		return err
	}

	if dec.processed[absPath] {
		return nil
	}
	dec.processed[absPath] = true

	f, err := os.Open(absPath)
	if err != nil {
		return err
	}
	defer f.Close()

	// 保存当前状态
	oldLexer := dec.p.l
	oldCur := dec.p.curToken
	oldPeek := dec.p.peekToken
	oldBasePath := dec.d.basePath

	// 设置新状态
	dec.d.basePath = filepath.Dir(absPath)
	l := newStreamLexer(f)
	defer putStreamLexer(l) // 修复泄漏
	dec.p.l = l
	dec.p.nextToken()
	dec.p.nextToken()

	err = dec.decodeBody(rv)

	// 恢复状态
	dec.d.basePath = oldBasePath
	dec.p.l = oldLexer
	dec.p.curToken = oldCur
	dec.p.peekToken = oldPeek

	if err != nil && err != io.EOF {
		return err
	}
	return nil
}

// decodeAssignStatement 解码赋值语句。
func (dec *StreamDecoder) decodeAssignStatement(rv reflect.Value) error {
	var identName string
	var field reflect.Value
	var tag wanfTag
	var ok bool

	if rv.Kind() == reflect.Map && rv.Type().Key().Kind() == reflect.String {
		identName = dec.safeString(dec.p.curToken.Literal)
	} else {
		field, tag, ok = findFieldAndTag(rv, dec.p.curToken.Literal)
	}

	dec.p.nextToken()              // 消费标识符
	if !dec.p.curTokenIs(ASSIGN) { // 更安全的检查方式：确保当前token是赋值符号
		return fmt.Errorf("wanf: expected '=' after identifier on line %d", dec.p.curToken.Line)
	}
	dec.p.nextToken() // 消费赋值符号

	if identName != "" {
		elemType := rv.Type().Elem()
		newElem := reflect.New(elemType).Elem()
		if err := dec.decodeValueTo(newElem); err != nil {
			return err
		}
		rv.SetMapIndex(reflect.ValueOf(identName), newElem)
		return nil
	}

	// 查找结构体字段及其标签
	if !ok {
		// 如果未找到字段，仍然需要消费token
		_, err := dec.evalExpressionOnTheFly()
		return err
	}

	if tag.KeyField != "" {
		val, err := dec.evalExpressionOnTheFly()
		if err != nil {
			return err
		}
		return dec.d.setMapFromList(field, val, tag.KeyField)
	}

	return dec.decodeValueTo(field)
}

func (dec *StreamDecoder) decodeValueTo(field reflect.Value) error {
	if field.Kind() == reflect.Pointer {
		if field.IsNil() {
			field.Set(reflect.New(field.Type().Elem()))
		}
		return dec.decodeValueTo(field.Elem())
	}

	switch dec.p.curToken.Type {
	case INT:
		val, err := strconv.ParseInt(BytesToString(dec.p.curToken.Literal), 0, 64)
		if err != nil {
			return err
		}
		dec.p.nextToken()
		kind := field.Kind()
		if kind >= reflect.Int && kind <= reflect.Int64 {
			field.SetInt(val)
			return nil
		}
		return dec.d.setInt(field, val)
	case FLOAT:
		val, err := strconv.ParseFloat(BytesToString(dec.p.curToken.Literal), 64)
		if err != nil {
			return err
		}
		dec.p.nextToken()
		kind := field.Kind()
		if kind == reflect.Float32 || kind == reflect.Float64 {
			field.SetFloat(val)
			return nil
		}
		return dec.d.setFloat(field, val)
	case STRING:
		val := dec.safeString(dec.p.curToken.Literal)
		dec.p.nextToken()
		if field.Kind() == reflect.String {
			field.SetString(val)
			return nil
		}
		return dec.d.setString(field, val)
	case BOOL:
		val, err := strconv.ParseBool(BytesToString(dec.p.curToken.Literal))
		if err != nil {
			return err
		}
		dec.p.nextToken()
		if field.Kind() == reflect.Bool {
			field.SetBool(val)
			return nil
		}
		return dec.d.setBool(field, val)
	case DUR:
		lit := dec.p.curToken.Literal
		kind := field.Kind()
		if kind == reflect.String {
			val := dec.safeString(lit)
			dec.p.nextToken()
			field.SetString(val)
			return nil
		}
		dur, err := time.ParseDuration(BytesToString(lit))
		if err != nil {
			return err
		}
		dec.p.nextToken()
		if (kind == reflect.Int64 || kind == reflect.Int) && field.Type() == durationType {
			field.SetInt(int64(dur))
			return nil
		}
		return dec.d.setField(field, dur)
	case DOLLAR_LBRACE:
		val, err := dec.evalVarExpressionOnTheFly()
		if err != nil {
			return err
		}
		if field.Kind() == reflect.Interface {
			field.Set(reflect.ValueOf(val))
			return nil
		}
		return dec.d.setField(field, val)
	case IDENT:
		if bytes.Equal(dec.p.curToken.Literal, []byte("env")) {
			val, err := dec.evalEnvExpressionOnTheFly()
			if err != nil {
				return err
			}
			return dec.d.setField(field, val)
		}
		// 如果是其他标识符且目标是接口，报错或按需处理
		return fmt.Errorf("wanf: unexpected identifier %q in expression on line %d", dec.p.curToken.Literal, dec.p.curToken.Line)
	case LBRACK:
		if field.Kind() == reflect.Slice {
			return dec.decodeListToSlice(field)
		}
		val, err := dec.decodeListLiteralOnTheFly()
		if err != nil {
			return err
		}
		return dec.d.setField(field, val)
	case LBRACE:
		if dec.p.peekTokenIs(LBRACK) {
			val, err := dec.decodeMapLiteralOnTheFly()
			if err != nil {
				return err
			}
			return dec.d.setField(field, val)
		}
		if field.Kind() == reflect.Map && field.Type().Key().Kind() == reflect.String {
			dec.p.nextToken() // {
			if field.IsNil() {
				field.Set(reflect.MakeMapWithSize(field.Type(), 4))
			}
			err := dec.decodeBody(field)
			if err != nil && err != io.EOF {
				return err
			}
			if !dec.p.curTokenIs(RBRACE) {
				return fmt.Errorf("wanf: expected '}' after map block on line %d", dec.p.curToken.Line)
			}
			dec.p.nextToken()
			return nil
		}
		if field.Kind() == reflect.Struct {
			dec.p.nextToken() // {
			err := dec.decodeBody(field)
			if err != nil && err != io.EOF {
				return err
			}
			if !dec.p.curTokenIs(RBRACE) {
				return fmt.Errorf("wanf: expected '}' after block on line %d", dec.p.curToken.Line)
			}
			dec.p.nextToken()
			return nil
		}
		val, err := dec.decodeBlockLiteralOnTheFly()
		if err != nil {
			return err
		}
		return dec.d.setField(field, val)
	}

	return fmt.Errorf("wanf: unexpected token %s for field of type %s", dec.p.curToken.Type, field.Type())
}

func (dec *StreamDecoder) decodeListToSlice(field reflect.Value) error {
	dec.p.nextToken() // [
	elemType := field.Type().Elem()

	if field.IsNil() || field.Cap() == 0 {
		field.Set(reflect.MakeSlice(field.Type(), 8, 8))
	}
	slice := field
	kind := elemType.Kind()

	i := 0
	for !dec.p.curTokenIs(RBRACK) && !dec.p.curTokenIs(EOF) {
		if i >= slice.Cap() {
			newCap := slice.Cap() * 2
			newSlice := reflect.MakeSlice(slice.Type(), i, newCap)
			reflect.Copy(newSlice, slice.Slice(0, i))
			slice = newSlice
			field.Set(slice)
		}
		if i >= slice.Len() {
			slice.SetLen(i + 1)
		}
		elem := slice.Index(i)

		// 针对常见简单类型进行优化，避免 reflect.New 和 reflect.ValueOf 产生的堆分配
		switch {
		case kind == reflect.String && dec.p.curTokenIs(STRING):
			elem.SetString(dec.safeString(dec.p.curToken.Literal))
			dec.p.nextToken()
		case (kind == reflect.Int || kind == reflect.Int64) && dec.p.curTokenIs(INT):
			val, _ := strconv.ParseInt(BytesToString(dec.p.curToken.Literal), 0, 64)
			elem.SetInt(val)
			dec.p.nextToken()
		case kind == reflect.Bool && dec.p.curTokenIs(BOOL):
			val, _ := strconv.ParseBool(BytesToString(dec.p.curToken.Literal))
			elem.SetBool(val)
			dec.p.nextToken()
		default:
			if err := dec.decodeValueTo(elem); err != nil {
				return err
			}
		}

		if dec.p.curTokenIs(COMMA) {
			dec.p.nextToken()
		} else if !dec.p.curTokenIs(RBRACK) {
			return fmt.Errorf("wanf: expected comma or ']' in list")
		}
		i++
	}
	if !dec.p.curTokenIs(RBRACK) {
		return fmt.Errorf("wanf: unclosed list")
	}
	field.SetLen(i)
	dec.p.nextToken()
	return nil
}

// decodeBlockStatement 解码块语句，现在负责消费末尾的'}'。
func (dec *StreamDecoder) decodeBlockStatement(rv reflect.Value) error {
	var blockName string
	var field reflect.Value
	var ok bool

	if rv.Kind() == reflect.Map && rv.Type().Key().Kind() == reflect.String {
		blockName = dec.safeString(dec.p.curToken.Literal)
	} else {
		field, _, ok = findFieldAndTag(rv, dec.p.curToken.Literal)
	}

	dec.p.nextToken() // 消费块名称

	var label string
	if dec.p.curTokenIs(STRING) { // 如果块带有字符串标签
		label = dec.safeString(dec.p.curToken.Literal)
		dec.p.nextToken() // 消费标签
	}

	if !dec.p.curTokenIs(LBRACE) { // 期望块标识符后跟'{'
		return fmt.Errorf("wanf: expected '{' after block identifier on line %d", dec.p.curToken.Line)
	}
	dec.p.nextToken() // 消费'{'

	if blockName != "" {
		elemType := rv.Type().Elem()
		newElem := reflect.New(elemType).Elem()
		if err := dec.decodeBody(newElem); err != nil && err != io.EOF {
			return err
		}
		if !dec.p.curTokenIs(RBRACE) {
			return fmt.Errorf("wanf: expected '}' to close block on line %d", dec.p.curToken.Line)
		}
		dec.p.nextToken()

		key := blockName
		if label != "" {
			key = label
		}
		rv.SetMapIndex(reflect.ValueOf(key), newElem)
		return nil
	}

	// 查找结构体字段及其标签
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
			field.Set(reflect.MakeMapWithSize(field.Type(), 4)) // 如果map为空，则初始化
		}
		mapElemType := field.Type().Elem() // 获取map元素类型

		if mapElemType.Kind() == reflect.Interface {
			if label == "" {
				m, err := dec.decodeBlockLiteralBodyOnTheFly()
				if err != nil {
					return err
				}
				for k, v := range m.(map[string]any) {
					field.SetMapIndex(reflect.ValueOf(k), reflect.ValueOf(v))
				}
				// decodeBlockLiteralBodyOnTheFly 已经消费了 '}'，所以我们直接返回
				return nil
			} else {
				m, err := dec.decodeBlockLiteralBodyOnTheFly()
				if err != nil {
					return err
				}
				field.SetMapIndex(reflect.ValueOf(label), reflect.ValueOf(m))
				return nil
			}
		}

		newElem := reflect.New(mapElemType).Elem() // 创建新的map元素实例
		// 解码map元素内部内容
		if err := dec.decodeBody(newElem); err != nil && err != io.EOF {
			return err
		}
		if label == "" { // map块需要一个标签作为key
			return fmt.Errorf("wanf: map block %q requires a label", blockName)
		}
		field.SetMapIndex(reflect.ValueOf(label), newElem) // 将解码后的元素设置到map中
	default: // 不支持的块解码类型
		return fmt.Errorf("wanf: block %q cannot be decoded into field of type %s", blockName, field.Type())
	}

	if !dec.p.curTokenIs(RBRACE) { // 期望块以'}'结束
		return fmt.Errorf("wanf: expected '}' to close block %q on line %d", blockName, dec.p.curToken.Line)
	}
	dec.p.nextToken() // 消费'}'
	return nil
}

// evalExpressionOnTheFly 实时评估表达式的值，并消费表达式的最后一个token。
func (dec *StreamDecoder) evalExpressionOnTheFly() (any, error) {
	var val any
	var err error

	switch dec.p.curToken.Type {
	case INT: // 整数
		val, err = strconv.ParseInt(BytesToString(dec.p.curToken.Literal), 0, 64)
	case FLOAT: // 浮点数
		val, err = strconv.ParseFloat(BytesToString(dec.p.curToken.Literal), 64)
	case STRING: // 字符串
		val = dec.safeString(dec.p.curToken.Literal)
	case BOOL: // 布尔值
		val, err = strconv.ParseBool(BytesToString(dec.p.curToken.Literal))
	case DUR: // 时间段 (duration)
		val, err = time.ParseDuration(BytesToString(dec.p.curToken.Literal))
	case DOLLAR_LBRACE:
		return dec.evalVarExpressionOnTheFly()
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

func (dec *StreamDecoder) evalVarExpressionOnTheFly() (any, error) {
	dec.p.nextToken() // ${
	if !dec.p.curTokenIs(IDENT) {
		return nil, fmt.Errorf("wanf: expected identifier in variable expression")
	}
	varName := dec.safeString(dec.p.curToken.Literal)
	dec.p.nextToken()
	if !dec.p.curTokenIs(RBRACE) {
		return nil, fmt.Errorf("wanf: expected '}' in variable expression")
	}
	dec.p.nextToken()

	val, ok := dec.d.vars[varName]
	if !ok {
		return nil, fmt.Errorf("wanf: variable %q not defined", varName)
	}
	return val, nil
}

// decodeListLiteralOnTheFly 实时解码列表字面量，并消费末尾的']'。
func (dec *StreamDecoder) decodeListLiteralOnTheFly() (any, error) {
	var list []any
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
func (dec *StreamDecoder) decodeBlockLiteralOnTheFly() (any, error) {
	dec.p.nextToken() // 消费'{'
	return dec.decodeBlockLiteralBodyOnTheFly()
}

// decodeBlockLiteralBodyOnTheFly 实时解码块字面量的主体部分，并消费末尾的'}'。
func (dec *StreamDecoder) decodeBlockLiteralBodyOnTheFly() (any, error) {
	m := make(map[string]any)

	for !dec.p.curTokenIs(RBRACE) && !dec.p.curTokenIs(EOF) {
		if dec.p.curTokenIs(COMMENT) || dec.p.curTokenIs(SEMICOLON) { // 忽略注释和分号
			dec.p.nextToken()
			continue
		}
		if !dec.p.curTokenIs(IDENT) { // 期望标识符作为块字面量的键
			return nil, fmt.Errorf("wanf: expected identifier as key in block literal (line %d, got %s)", dec.p.curToken.Line, dec.p.curToken.Type)
		}
		key := dec.safeString(dec.p.curToken.Literal) // 获取键

		dec.p.nextToken() // 消费标识符
		if dec.p.curTokenIs(ASSIGN) {
			dec.p.nextToken() // 消费赋值符号
			val, err := dec.evalExpressionOnTheFly()
			if err != nil {
				return nil, err
			}
			m[key] = val
		} else if dec.p.curTokenIs(LBRACE) {
			val, err := dec.decodeBlockLiteralOnTheFly()
			if err != nil {
				return nil, err
			}
			m[key] = val
		} else {
			return nil, fmt.Errorf("wanf: expected '=' or '{' after key %q in block literal (line %d)", key, dec.p.curToken.Line)
		}
		// 此处不需要nextToken，因为evalExpressionOnTheFly已经处理了。
	}
	if !dec.p.curTokenIs(RBRACE) { // 块字面量未闭合
		return nil, fmt.Errorf("wanf: unclosed block literal")
	}
	dec.p.nextToken() // 消费'}'
	return m, nil
}

// decodeMapLiteralOnTheFly 实时解码map字面量（{ [key=value, ...] } 形式），并消费末尾的'}'。
func (dec *StreamDecoder) decodeMapLiteralOnTheFly() (any, error) {
	m := make(map[string]any)
	dec.p.nextToken() // 消费'{'
	dec.p.nextToken() // 消费'['

	for !dec.p.curTokenIs(RBRACK) && !dec.p.curTokenIs(EOF) {
		if !dec.p.curTokenIs(IDENT) { // 期望标识符作为map字面量的键
			return nil, fmt.Errorf("wanf: expected identifier as key in map literal")
		}
		key := dec.safeString(dec.p.curToken.Literal) // 获取键
		dec.p.nextToken()                     // 消费标识符
		if !dec.p.curTokenIs(ASSIGN) {        // 期望键后跟'='
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
func (dec *StreamDecoder) evalEnvExpressionOnTheFly() (any, error) {
	dec.p.nextToken()              // 消费'env'
	if !dec.p.curTokenIs(LPAREN) { // 期望'('
		return nil, fmt.Errorf("wanf: expected '(' after env")
	}
	dec.p.nextToken() // 消费'('

	if !dec.p.curTokenIs(STRING) { // 期望env()的参数是字符串
		return nil, fmt.Errorf("wanf: expected string argument for env()")
	}
	envVarName := dec.safeString(dec.p.curToken.Literal) // 获取环境变量名
	dec.p.nextToken()                            // 消费环境变量名字符串

	var val string
	var found bool

	// 检查是否有默认值
	if dec.p.curTokenIs(COMMA) {
		dec.p.nextToken()              // 消费','
		if !dec.p.curTokenIs(STRING) { // 期望默认值是字符串
			return nil, fmt.Errorf("wanf: expected string for env() default value")
		}
		defaultValue := dec.safeString(dec.p.curToken.Literal) // 获取默认值
		dec.p.nextToken()                              // 消费默认值字符串

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
