package wanf

import (
	"fmt"
	"io"
	"reflect"
	"strconv"
	"sync"
	"time"
	"unsafe"
)

type NeoDecoder struct {
	l           *NeoLexer
	err         error
	stringCache map[string]string
}

var neoDecoderPool = sync.Pool{
	New: func() any {
		return &NeoDecoder{
			stringCache: make(map[string]string),
		}
	},
}

func NewNeoDecoder(r io.Reader) *NeoDecoder {
	dec := neoDecoderPool.Get().(*NeoDecoder)
	dec.l = NewNeoLexer(r)
	dec.err = nil
	return dec
}

func NewNeoDecoderBytes(data []byte) *NeoDecoder {
	dec := neoDecoderPool.Get().(*NeoDecoder)
	if dec.l == nil {
		dec.l = NewNeoLexer(nil)
	} else {
		dec.l.reader = nil
	}
	dec.l.SetInput(data)
	dec.err = nil
	return dec
}

func (dec *NeoDecoder) Close() {
	dec.l.Close()
	if len(dec.stringCache) > 1024 {
		dec.stringCache = make(map[string]string)
	}
	neoDecoderPool.Put(dec)
}

func (dec *NeoDecoder) Decode(v any) error {
	rv := reflect.ValueOf(v)
	if rv.Kind() != reflect.Pointer || rv.IsNil() {
		return fmt.Errorf("NeoDecoder: Decode requires a non-nil pointer")
	}
	rv = rv.Elem()
	if rv.Kind() != reflect.Struct {
		return fmt.Errorf("NeoDecoder: only structs are supported")
	}
	info := getNeoStructInfo(rv.Type())
	ptr := unsafe.Pointer(rv.UnsafeAddr())
	return dec.decodeStruct(info, ptr)
}

func (dec *NeoDecoder) decodeStruct(info *neoStructInfo, ptr unsafe.Pointer) error {
	for {
		tok := dec.l.nextToken()
		if tok.Type == EOF || tok.Type == RBRACE {
			break
		}
		if tok.Type != IDENT {
			continue
		}
		f := info.findField(tok.Literal)
		if f == nil {
			f = info.findFieldCaseInsensitive(tok.Literal)
		}

		if f == nil {
			dec.skipField()
			continue
		}
		fieldPtr := unsafe.Pointer(uintptr(ptr) + f.offset)
		next := dec.l.nextToken()

		if next.Type == ASSIGN {
			valTok := dec.l.nextToken()
			if valTok.Type == LBRACE && f.kind == reflect.Struct {
				dec.decodeStructInto(f, fieldPtr)
			} else {
				dec.decodeValueWithToken(f, fieldPtr, valTok)
			}
		} else if next.Type == LBRACE {
			if f.kind == reflect.Struct {
				dec.decodeStructInto(f, fieldPtr)
			} else {
				dec.skipValue()
			}
		}
	}
	return dec.err
}

func (dec *NeoDecoder) decodeStructInto(f *neoField, fieldPtr unsafe.Pointer) {
	targetPtr := fieldPtr
	if f.isPointer {
		if *(*unsafe.Pointer)(fieldPtr) == nil {
			newVal := reflect.New(f.elemType.Elem())
			*(*unsafe.Pointer)(fieldPtr) = unsafe.Pointer(newVal.Pointer())
		}
		targetPtr = *(*unsafe.Pointer)(fieldPtr)
	}
	dec.decodeStruct(f.structInfo, targetPtr)
}

func (dec *NeoDecoder) decodeValueWithToken(f *neoField, ptr unsafe.Pointer, tok Token) {
	targetPtr := ptr
	if f.isPointer {
		if *(*unsafe.Pointer)(ptr) == nil {
			newVal := reflect.New(f.elemType.Elem())
			*(*unsafe.Pointer)(ptr) = unsafe.Pointer(newVal.Pointer())
		}
		targetPtr = *(*unsafe.Pointer)(ptr)
	}

	if f.elemType == durationType || (f.isPointer && f.elemType.Elem() == durationType) {
		d, _ := time.ParseDuration(BytesToString(tok.Literal))
		*(*time.Duration)(targetPtr) = d
		return
	}

	switch f.kind {
	case reflect.String:
		s, ok := dec.stringCache[BytesToString(tok.Literal)]
		if !ok {
			s = string(tok.Literal)
			dec.stringCache[s] = s
		}
		*(*string)(targetPtr) = s
	case reflect.Int:
		*(*int)(targetPtr) = int(dec.parseInt(tok.Literal))
	case reflect.Int64:
		*(*int64)(targetPtr) = dec.parseInt(tok.Literal)
	case reflect.Bool:
		if len(tok.Literal) == 4 && tok.Literal[0] == 't' {
			*(*bool)(targetPtr) = true
		} else {
			*(*bool)(targetPtr) = false
		}
	case reflect.Float64:
		f64, _ := strconv.ParseFloat(BytesToString(tok.Literal), 64)
		*(*float64)(targetPtr) = f64
	case reflect.Float32:
		f64, _ := strconv.ParseFloat(BytesToString(tok.Literal), 32)
		*(*float32)(targetPtr) = float32(f64)
	}
}

func (dec *NeoDecoder) parseInt(b []byte) int64 {
	if len(b) == 0 { return 0 }
	neg := false
	if b[0] == '-' { neg = true; b = b[1:] }
	var res int64
	for _, c := range b {
		if c < '0' || c > '9' { break }
		res = res*10 + int64(c-'0')
	}
	if neg { return -res }
	return res
}

func (dec *NeoDecoder) skipField() {
	next := dec.l.nextToken()
	if next.Type == ASSIGN {
		dec.skipValue()
	} else if next.Type == LBRACE {
		dec.skipBraces()
	}
}

func (dec *NeoDecoder) skipValue() {
	tok := dec.l.nextToken()
	if tok.Type == LBRACE {
		dec.skipBraces()
	}
}

func (dec *NeoDecoder) skipBraces() {
	depth := 1
	for depth > 0 {
		t := dec.l.nextToken()
		if t.Type == LBRACE { depth++ } else if t.Type == RBRACE { depth-- } else if t.Type == EOF { break }
	}
}
