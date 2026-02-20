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
			dec.skipValue()
			continue
		}
		fieldPtr := unsafe.Pointer(uintptr(ptr) + f.offset)
		next := dec.l.nextToken()
		if next.Type == ASSIGN {
			dec.decodeValue(f, fieldPtr)
		} else if next.Type == LBRACE {
			if f.isBlock && f.structInfo != nil {
				dec.decodeStruct(f.structInfo, fieldPtr)
			} else {
				dec.skipValue()
			}
		}
	}
	return dec.err
}

func (dec *NeoDecoder) decodeValue(f *neoField, ptr unsafe.Pointer) {
	tok := dec.l.nextToken()
	if f.elemType == durationType && tok.Type == DUR {
		d, _ := time.ParseDuration(BytesToString(tok.Literal))
		*(*time.Duration)(ptr) = d
		return
	}
	switch f.kind {
	case reflect.String:
		s, ok := dec.stringCache[BytesToString(tok.Literal)]
		if !ok {
			s = string(tok.Literal)
			dec.stringCache[s] = s
		}
		*(*string)(ptr) = s
	case reflect.Int:
		*(*int)(ptr) = int(dec.parseInt(tok.Literal))
	case reflect.Int64:
		*(*int64)(ptr) = dec.parseInt(tok.Literal)
	case reflect.Bool:
		if len(tok.Literal) == 4 && tok.Literal[0] == 't' {
			*(*bool)(ptr) = true
		} else {
			*(*bool)(ptr) = false
		}
	case reflect.Float64:
		f64, _ := strconv.ParseFloat(BytesToString(tok.Literal), 64)
		*(*float64)(ptr) = f64
	case reflect.Float32:
		f64, _ := strconv.ParseFloat(BytesToString(tok.Literal), 32)
		*(*float32)(ptr) = float32(f64)
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

func (dec *NeoDecoder) skipValue() {
	tok := dec.l.nextToken()
	if tok.Type == LBRACE {
		depth := 1
		for depth > 0 {
			t := dec.l.nextToken()
			if t.Type == LBRACE { depth++ } else if t.Type == RBRACE { depth-- } else if t.Type == EOF { break }
		}
	}
}
