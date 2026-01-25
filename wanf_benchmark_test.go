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

// BenchmarkLexer measures the performance of tokenizing a wanf file.
func BenchmarkLexer(b *testing.B) {
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

// BenchmarkParser measures the performance of parsing a wanf file (lexing + parsing).
func BenchmarkParser(b *testing.B) {
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

// BenchmarkFormat measures the end-to-end performance of linting and formatting.
func BenchmarkFormat(b *testing.B) {
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

// BenchmarkDecode measures the performance of decoding a wanf file into a Go struct.
func BenchmarkDecode(b *testing.B) {
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
		// The Decode function is what we are testing.
		// With the cache, this should be much faster on subsequent runs.
		_ = Decode(benchmarkWanfData, &cfg)
	}
}

// BenchmarkEncode measures the performance of encoding a Go struct into wanf format.
func BenchmarkEncode(b *testing.B) {
	if benchmarkWanfData == nil {
		b.Skip("Cannot read benchmark data file")
	}
	// Create a representative config struct by decoding the benchmark file once.
	// This ensures we are encoding the same data that we decode.
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

// BenchmarkDecode_SimpleFile measures the performance of the standard decoder on simple data.
// This provides a direct comparison to BenchmarkStreamDecode.
func BenchmarkDecode_SimpleFile(b *testing.B) {
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

// BenchmarkStreamDecode 测试流式解码器的性能.
func BenchmarkStreamDecode(b *testing.B) {
	if benchmarkStreamWanfData == nil {
		b.Skip("Cannot read stream benchmark data file")
	}
	b.ReportAllocs()
	b.ResetTimer()

	reader := bytes.NewReader(benchmarkStreamWanfData)

	for i := 0; i < b.N; i++ {
		var cfg benchmarkConfig
		reader.Seek(0, io.SeekStart)
		dec, err := NewStreamDecoder(reader)
		if err != nil {
			b.Fatalf("NewStreamDecoder failed during benchmark: %v", err)
		}
		err = dec.Decode(&cfg)
		if err != nil {
			b.Fatalf("Decode failed during benchmark: %v", err)
		}
	}
}

func BenchmarkStreamEncode(b *testing.B) {
	if benchmarkWanfData == nil {
		b.Skip("Cannot read benchmark data file")
	}
	// Create a representative config struct by decoding the benchmark file once.
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
		// Use io.Discard to benchmark the streaming performance without writing to a buffer.
		enc := NewStreamEncoder(io.Discard)
		_ = enc.Encode(&config) // Using default options for benchmark
	}
}

func BenchmarkLexerDurationSingle(b *testing.B) {
	input := []byte("10s")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		l := NewLexer(input)
		l.NextToken()
	}
}

func BenchmarkLexerDurationCompound(b *testing.B) {
	input := []byte("1h30m45s")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		l := NewLexer(input)
		l.NextToken()
	}
}

func BenchmarkStreamLexerDurationSingle(b *testing.B) {
	input := []byte("10s")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		l := newStreamLexer(bytes.NewReader(input))
		l.NextToken()
	}
}

func BenchmarkStreamLexerDurationCompound(b *testing.B) {
	input := []byte("1h30m45s")
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		l := newStreamLexer(bytes.NewReader(input))
		l.NextToken()
	}
}
