package wanf

import (
	"bytes"
	"io"
	"testing"
)

func BenchmarkLexer_Neo(b *testing.B) {
	if benchmarkWanfData == nil {
		b.Skip("Cannot read benchmark data file")
	}
	reader := bytes.NewReader(benchmarkWanfData)
	b.ResetTimer()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		reader.Reset(benchmarkWanfData)
		l := NewNeoLexer(reader)
		for {
			tok := l.nextToken()
			if tok.Type == EOF {
				break
			}
		}
		l.Close()
	}
}

func BenchmarkLexerDurationSingle_Neo(b *testing.B) {
	input := []byte("10s")
	reader := bytes.NewReader(input)
	b.ResetTimer()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		reader.Reset(input)
		l := NewNeoLexer(reader)
		l.nextToken()
		l.Close()
	}
}

func BenchmarkLexerDurationCompound_Neo(b *testing.B) {
	input := []byte("1h30m45s")
	reader := bytes.NewReader(input)
	b.ResetTimer()
	b.ReportAllocs()
	for i := 0; i < b.N; i++ {
		reader.Reset(input)
		l := NewNeoLexer(reader)
		l.nextToken()
		l.Close()
	}
}

func BenchmarkDecodeComplex_Neo(b *testing.B) {
	if benchmarkWanfData == nil {
		b.Skip("Cannot read benchmark data file")
	}
	reader := bytes.NewReader(benchmarkWanfData)
	b.ResetTimer()
	b.ReportAllocs()

	for i := 0; i < b.N; i++ {
		var cfg benchmarkConfig
		reader.Reset(benchmarkWanfData)
		dec := NewNeoDecoder(reader)
		_ = dec.Decode(&cfg)
		dec.Close()
	}
}

func BenchmarkEncodeComplex_Neo(b *testing.B) {
	if benchmarkWanfData == nil {
		b.Skip("Cannot read benchmark data file")
	}
	var config benchmarkConfig
	_ = Decode(benchmarkWanfData, &config)

	b.ResetTimer()
	b.ReportAllocs()

	for i := 0; i < b.N; i++ {
		enc := NewNeoEncoder(io.Discard)
		_ = enc.Encode(&config)
		enc.Close()
	}
}

func BenchmarkDecodeSimple_Neo(b *testing.B) {
	if benchmarkStreamWanfData == nil {
		b.Skip("Cannot read stream benchmark data file")
	}
	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		var cfg benchmarkConfig
		dec := NewNeoDecoderBytes(benchmarkStreamWanfData)
		_ = dec.Decode(&cfg)
		dec.Close()
	}
}

func BenchmarkEncodeSimple_Neo(b *testing.B) {
	if benchmarkStreamWanfData == nil {
		b.Skip("Cannot read stream benchmark data file")
	}
	var config benchmarkConfig
	_ = Decode(benchmarkStreamWanfData, &config)

	b.ResetTimer()
	b.ReportAllocs()

	for i := 0; i < b.N; i++ {
		enc := NewNeoEncoder(io.Discard)
		_ = enc.Encode(&config)
		enc.Close()
	}
}

func BenchmarkDecodeComplex_NeoUnmarshal(b *testing.B) {
	if benchmarkWanfData == nil {
		b.Skip("Cannot read benchmark data file")
	}
	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		var cfg benchmarkConfig
		_ = NeoUnmarshal(benchmarkWanfData, &cfg)
	}
}

func BenchmarkLexer_NeoFast(b *testing.B) {
	if benchmarkWanfData == nil {
		b.Skip("Cannot read benchmark data file")
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		l := NewNeoLexer(nil)
		l.SetInput(benchmarkWanfData)
		for {
			tok := l.nextToken()
			if tok.Type == EOF {
				break
			}
		}
		l.Close()
	}
}
