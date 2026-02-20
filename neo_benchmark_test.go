package wanf

import (
	"io"
	"testing"
)

type neoSimpleConfig struct {
	Name    string `wanf:"name"`
	Port    int    `wanf:"port"`
	Active  bool   `wanf:"active"`
}

func BenchmarkNeoEncode_Simple(b *testing.B) {
	cfg := neoSimpleConfig{Name: "Neo", Port: 8080, Active: true}
	b.ResetTimer()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		enc := NewNeoEncoder(io.Discard)
		enc.Encode(&cfg)
		enc.Close()
	}
}

func BenchmarkNeoDecode_Simple(b *testing.B) {
	data := []byte(`name = "Neo"; port = 8080; active = true`)
	b.ResetTimer()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		var cfg neoSimpleConfig
		dec := NewNeoDecoderBytes(data)
		dec.Decode(&cfg)
		dec.Close()
	}
}

func BenchmarkNeoEncode_Complex(b *testing.B) {
	var config benchmarkConfig
	NeoUnmarshal(benchmarkWanfData, &config)

	b.ResetTimer()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		enc := NewNeoEncoder(io.Discard)
		enc.Encode(&config)
		enc.Close()
	}
}
