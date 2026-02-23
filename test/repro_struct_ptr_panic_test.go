package wanf_test

import (
	"github.com/WJQSERVER/wanf"
	"testing"
)

type Sub struct {
	A int `wanf:"a"`
}

type Root struct {
	SubPtr *Sub `wanf:"sub_ptr"`
}

func TestStructPtrPanic(t *testing.T) {
	s := Sub{A: 1}
	r := Root{SubPtr: &s}
	_, err := wanf.Marshal(r)
	if err != nil {
		t.Fatal(err)
	}
}
