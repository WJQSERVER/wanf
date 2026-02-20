package wanf

import (
	"fmt"
	"io"
	"reflect"
	"strconv"
	"sync"
	"unsafe"
)

type NeoDecoder struct {
	l   *NeoLexer
	err error
}

var neoDecoderPool = sync.Pool{
	New: func() any {
		return &NeoDecoder{}
	},
}

func NewNeoDecoder(r io.Reader) *NeoDecoder {
	dec := neoDecoderPool.Get().(*NeoDecoder)
	dec.l = NewNeoLexer(r)
	dec.err = nil
	return dec
}

func (dec *NeoDecoder) Close() {
	dec.l.Close()
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

		nameBytes := tok.Literal
		f, ok := info.byName[BytesToString(nameBytes)]
		if !ok {
			// Case-insensitive fallback
			var buf [64]byte
			var b []byte
			if len(nameBytes) <= 64 {
				b = buf[:len(nameBytes)]
			} else {
				b = make([]byte, len(nameBytes))
			}
			for i := range nameBytes {
				c := nameBytes[i]
				if c >= "A"[0] && c <= "Z"[0] {
					b[i] = c + ("a"[0] - "A"[0])
				} else {
					b[i] = c
				}
			}
			f, ok = info.byName[BytesToString(b)]
		}
		if !ok {
			dec.skipValue()
			continue
		}

		fieldPtr := unsafe.Pointer(uintptr(ptr) + f.offset)
		if f.isPtr {
			if *(*unsafe.Pointer)(fieldPtr) == nil {
				rv := reflect.New(f.elemType.Elem())
				*(*unsafe.Pointer)(fieldPtr) = unsafe.Pointer(rv.Pointer())
			}
			fieldPtr = *(*unsafe.Pointer)(fieldPtr)
		}

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
	switch f.kind {
	case reflect.String:
		// Safe copy. If we wanted 0-alloc we'd need to point to input buffer.
		// However, for Neo, we might use a pool for strings.
		*(*string)(ptr) = BytesToString(tok.Literal)
	case reflect.Int:
		i64, _ := strconv.ParseInt(BytesToString(tok.Literal), 10, 64)
		*(*int)(ptr) = int(i64)
	case reflect.Int64:
		i64, _ := strconv.ParseInt(BytesToString(tok.Literal), 10, 64)
		*(*int64)(ptr) = i64
	case reflect.Float64:
		f64, _ := strconv.ParseFloat(BytesToString(tok.Literal), 64)
		*(*float64)(ptr) = f64
	case reflect.Bool:
		if len(tok.Literal) == 4 && BytesToString(tok.Literal) == "true" {
			*(*bool)(ptr) = true
		} else {
			*(*bool)(ptr) = false
		}
	}
}

func (dec *NeoDecoder) skipValue() {
	tok := dec.l.nextToken()
	if tok.Type == LBRACE {
		depth := 1
		for depth > 0 {
			t := dec.l.nextToken()
			if t.Type == LBRACE {
				depth++
			} else if t.Type == RBRACE {
				depth--
			} else if t.Type == EOF {
				break
			}
		}
	}
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
