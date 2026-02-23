# Neo - WANF 极速编解码器 (Experimental)

Neo 是 WANF 库中一个实验性的极致性能编解码器。它通过使用 `unsafe` 内存偏移访问、零分配词法分析以及预计算的字段映射哈希表，旨在提供接近原生 C 速度的序列化和反序列化体验。

## 1. 核心特性

- **极致性能**: 编码过程实现 0 分配，解码过程分配极低。
- **Unsafe 优化**: 直接通过字段偏移量访问结构体成员，绕过 `reflect.Value` 的大部分开销。
- **大小写不敏感匹配**: 解码时支持字段名的大小写不敏感匹配，同时保持高性能。
- **支持常用类型**:
    - 基础类型: `int`, `int64`, `float64`, `bool`, `string`
    - 时间处理: `time.Duration` (支持标准字符串格式如 `10s`, `-5m`)
    - 结构化数据: 嵌套结构体 (Nested Structs)
    - 指针支持: 支持基础类型指针和结构体指针，自动处理解引用和内存分配。
- **功能特性**:
    - 支持 `wanf` 结构体标签。
    - 支持 `omitempty` 选项（在编码时跳过零值字段）。
    - 支持紧凑的“缩写块”语法 (`key { ... }`)。

## 2. 使用方式

Neo 提供了多种灵活的使用接口，满足不同的场景需求。

### 2.1 简单接口 (Marshal/Unmarshal)

最简单快捷的使用方式，适用于处理整个字节切片。

```go
type Config struct {
    Name string `wanf:"name"`
    Age  int    `wanf:"age"`
}

cfg := Config{Name: "Neo", Age: 30}

// 序列化
data, err := wanf.NeoMarshal(&cfg)

// 反序列化
var decoded Config
err = wanf.NeoUnmarshal(data, &decoded)
```

### 2.2 流式编码 (Encoder)

适用于需要将数据直接写入 `io.Writer` (如文件或网络连接) 的场景。

```go
func saveConfig(w io.Writer, cfg *Config) error {
    enc := wanf.NewNeoEncoder(w)
    defer enc.Close()
    return enc.Encode(cfg)
}
```

### 2.3 流式解码 (Decoder)

适用于从 `io.Reader` 逐条读取或解析大型文件的场景。

```go
func loadConfig(r io.Reader) (*Config, error) {
    dec := wanf.NewNeoDecoder(r)
    defer dec.Close()

    var cfg Config
    if err := dec.Decode(&cfg); err != nil {
        return nil, err
    }
    return &cfg, nil
}
```

### 2.4 高性能字节流解码 (NewNeoDecoderBytes)

如果您已经拥有数据的 `[]byte` 切片，使用 `NewNeoDecoderBytes` 可以通过禁用流式模式来进一步提升解码速度。

```go
dec := wanf.NewNeoDecoderBytes(data)
defer dec.Close()
err := dec.Decode(&cfg)
```

## 3. 性能优化建议

1. **传递指针**: `NeoEncoder.Encode` 要求传入的值必须是可取地址的（通常传递指针），以便利用 `UnsafeAddr()` 进行零分配字段访问。
2. **重用对象**: Neo 内部使用了 `sync.Pool` 来重用编码器和解码器实例，因此在高并发场景下性能表现非常出色。
3. **字段预计算**: Neo 会在第一次遇到某个结构体类型时缓存其字段偏移和哈希表，后续操作将直接使用缓存。

## 4. 注意事项与限制 (Experimental)

- **实验性状态**: Neo 目前仍处于实验阶段，虽然经过了基准测试和回归测试，但在复杂边缘场景下可能存在限制。
- **顶级对象**: 目前 Neo 主要针对顶级结构体进行优化。
- **集合支持**: 对 `Slice` 和 `Map` 的深度支持仍在持续完善中。

如果您追求极致的启动性能和最小的内存分配，Neo 是您的理想选择。
