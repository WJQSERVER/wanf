package wanf

import (
	"bytes"
	"io"
	"os"
	"testing"
)

// Benchmark data - a reasonably complex wanf file content.
var benchmarkWanfData, _ = os.ReadFile("testfile/example.wanf")
var benchmarkStreamWanfData, _ = os.ReadFile("testfile/benchmark_stream.wanf")

// BenchmarkLexer_Base measures the performance of tokenizing a wanf file.
func BenchmarkLexer_Base(b *testing.B) {
	if benchmarkWanfData == nil {
		b.Skip("Cannot read benchmark data file")
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		l := NewLexer(benchmarkWanfData)
		for {
			tok := l.NextToken()
			if tok.Type == EOF {
				break
			}
		}
	}
}

func BenchmarkLexer_Stream(b *testing.B) {
	if benchmarkWanfData == nil {
		b.Skip("Cannot read benchmark data file")
	}
	reader := bytes.NewReader(benchmarkWanfData)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		reader.Reset(benchmarkWanfData)
		l := newStreamLexer(reader)
		for {
			tok := l.NextToken()
			if tok.Type == EOF {
				break
			}
		}
		putStreamLexer(l)
	}
}

// BenchmarkParser_Base measures the performance of parsing a wanf file (lexing + parsing).
func BenchmarkParser_Base(b *testing.B) {
	if benchmarkWanfData == nil {
		b.Skip("Cannot read benchmark data file")
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		l := NewLexer(benchmarkWanfData)
		p := NewParser(l)
		p.ParseProgram()
	}
}

// BenchmarkFormat_Base measures the end-to-end performance of linting and formatting.
func BenchmarkFormat_Base(b *testing.B) {
	if benchmarkWanfData == nil {
		b.Skip("Cannot read benchmark data file")
	}
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		program, _ := Lint(benchmarkWanfData)
		Format(program, FormatOptions{Style: StyleBlockSorted, EmptyLines: true})
	}
}

// unified benchmark struct, matching testfile/example.wanf
type benchmarkConfig struct {
	Application struct {
		Name            string   `wanf:"name"`
		Version         float64  `wanf:"version"`
		DebugMode       bool     `wanf:"debug_mode"`
		MaxJobs         int      `wanf:"max_concurrent_jobs"`
		ShutdownTimeout string   `wanf:"shutdown_timeout"`
		Host            string   `wanf:"host"`
		AllowedOrigins  []string `wanf:"allowed_origins"`
	} `wanf:"application"`
	Database struct {
		Host string `wanf:"host"`
		Port int    `wanf:"port"`
	} `wanf:"database"`
	Logging struct {
		Level    string `wanf:"level"`
		Template string `wanf:"format_template"`
	} `wanf:"logging"`
	Server map[string]struct {
		Address    string `wanf:"address"`
		Protocol   string `wanf:"protocol"`
		MaxStreams int    `wanf:"max_streams,omitempty"`
	} `wanf:"server"`
	FeatureFlags []string `wanf:"feature_flags"`
	Middleware   []struct {
		ID        string `wanf:"id"`
		Enabled   bool   `wanf:"enabled"`
		JWTIssuer string `wanf:"jwt_issuer,omitempty"`
	} `wanf:"middleware"`
}

// BenchmarkDecodeComplex_Base measures the performance of decoding a wanf file into a Go struct.
func BenchmarkDecodeComplex_Base(b *testing.B) {
	if benchmarkWanfData == nil {
		b.Skip("Cannot read benchmark data file")
	}

	// Pre-populate the cache once before the benchmark loop, as it's a one-time cost.
	var cfg benchmarkConfig
	dec, err := NewDecoder(bytes.NewReader(benchmarkWanfData), WithBasePath("testfile"))
	if err != nil {
		b.Fatalf("Failed to create decoder for benchmark setup: %v", err)
	}
	_ = dec.Decode(&cfg)

	b.ResetTimer()
	b.ReportAllocs()

	for i := 0; i < b.N; i++ {
		var cfg benchmarkConfig
		_ = Decode(benchmarkWanfData, &cfg)
	}
}

func BenchmarkDecodeComplex_Stream(b *testing.B) {
	if benchmarkWanfData == nil {
		b.Skip("Cannot read benchmark data file")
	}
	reader := bytes.NewReader(benchmarkWanfData)
	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		var cfg benchmarkConfig
		reader.Reset(benchmarkWanfData)
		dec, _ := NewStreamDecoder(reader)
		_ = dec.Decode(&cfg)
		dec.Close()
	}
}

// BenchmarkEncodeComplex_Base measures the performance of encoding a Go struct into wanf format.
func BenchmarkEncodeComplex_Base(b *testing.B) {
	if benchmarkWanfData == nil {
		b.Skip("Cannot read benchmark data file")
	}
	var config benchmarkConfig
	dec, err := NewDecoder(bytes.NewReader(benchmarkWanfData), WithBasePath("testfile"))
	if err != nil {
		b.Fatalf("Failed to create decoder for benchmark setup: %v", err)
	}
	err = dec.Decode(&config)
	if err != nil {
		b.Fatalf("Failed to decode benchmark data for encoder setup: %v", err)
	}

	b.ResetTimer()
	b.ReportAllocs()

	for i := 0; i < b.N; i++ {
		_, _ = Marshal(&config)
	}
}

func BenchmarkEncodeComplex_Stream(b *testing.B) {
	if benchmarkWanfData == nil {
		b.Skip("Cannot read benchmark data file")
	}
	var config benchmarkConfig
	dec, err := NewDecoder(bytes.NewReader(benchmarkWanfData), WithBasePath("testfile"))
	if err != nil {
		b.Fatalf("Failed to create decoder for benchmark setup: %v", err)
	}
	err = dec.Decode(&config)
	if err != nil {
		b.Fatalf("Failed to decode benchmark data for encoder setup: %v", err)
	}

	b.ResetTimer()
	b.ReportAllocs()

	for i := 0; i < b.N; i++ {
		enc := NewStreamEncoder(io.Discard)
		_ = enc.Encode(&config)
	}
}

func BenchmarkDecodeSimple_Base(b *testing.B) {
	if benchmarkStreamWanfData == nil {
		b.Skip("Cannot read stream benchmark data file")
	}
	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		var cfg benchmarkConfig
		err := Decode(benchmarkStreamWanfData, &cfg)
		if err != nil {
			b.Fatalf("Decode with simple data failed during benchmark: %v", err)
		}
	}
}

func BenchmarkDecodeSimple_Stream(b *testing.B) {
	if benchmarkStreamWanfData == nil {
		b.Skip("Cannot read stream benchmark data file")
	}
	reader := bytes.NewReader(benchmarkStreamWanfData)
	b.ReportAllocs()
	b.ResetTimer()

	for i := 0; i < b.N; i++ {
		var cfg benchmarkConfig
		reader.Reset(benchmarkStreamWanfData)
		dec, err := NewStreamDecoder(reader)
		if err != nil {
			b.Fatalf("NewStreamDecoder failed during benchmark: %v", err)
		}
		err = dec.Decode(&cfg)
		dec.Close()
	}
}

func BenchmarkEncodeSimple_Base(b *testing.B) {
	if benchmarkStreamWanfData == nil {
		b.Skip("Cannot read stream benchmark data file")
	}
	var config benchmarkConfig
	_ = Decode(benchmarkStreamWanfData, &config)

	b.ResetTimer()
	b.ReportAllocs()

	for i := 0; i < b.N; i++ {
		_, _ = Marshal(&config)
	}
}

func BenchmarkEncodeSimple_Stream(b *testing.B) {
	if benchmarkStreamWanfData == nil {
		b.Skip("Cannot read stream benchmark data file")
	}
	var config benchmarkConfig
	_ = Decode(benchmarkStreamWanfData, &config)

	b.ResetTimer()
	b.ReportAllocs()

	for i := 0; i < b.N; i++ {
		enc := NewStreamEncoder(io.Discard)
		_ = enc.Encode(&config)
	}
}

func BenchmarkLexerDurationSingle_Base(b *testing.B) {
	input := []byte("10s")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		l := NewLexer(input)
		l.NextToken()
	}
}

func BenchmarkLexerDurationCompound_Base(b *testing.B) {
	input := []byte("1h30m45s")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		l := NewLexer(input)
		l.NextToken()
	}
}

func BenchmarkLexerDurationSingle_Stream(b *testing.B) {
	input := []byte("10s")
	reader := bytes.NewReader(input)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		reader.Reset(input)
		l := newStreamLexer(reader)
		l.NextToken()
		putStreamLexer(l)
	}
}

func BenchmarkLexerDurationCompound_Stream(b *testing.B) {
	input := []byte("1h30m45s")
	reader := bytes.NewReader(input)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		reader.Reset(input)
		l := newStreamLexer(reader)
		l.NextToken()
		putStreamLexer(l)
	}
}
