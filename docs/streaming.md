# 流式 API (Streaming API)

WANF 提供了 `StreamDecoder` 和 `StreamEncoder`，用于处理大文件或对性能有极高要求的场景。

## 1. StreamDecoder (流式解码)

`StreamDecoder` 直接从 `io.Reader` 读取并解析配置。

### 1.1 特性
- **功能对齐**: 尽管是流式处理，但它支持 `var` 声明、`import` 语句（带循环检测）和变量替换 (`${...}`)。
- **低内存占用**: 通过池化技术和即时解析，极大地减少了内存分配。
- **本地字符串缓存**: 内部维护一个字符串缓存，用于重用标识符和常用值，进一步降低分配。

### 1.2 使用示例
```go
reader, _ := os.Open("config.wanf")
dec, _ := wanf.NewStreamDecoder(reader)
defer dec.Close() // 必须调用 Close 以释放资源回池

var cfg Config
err := dec.Decode(&cfg)
```

### 1.3 注意事项
- **显式资源管理**: 必须调用 `Close()`。
- **Buffer 污染规避**: `StreamLexer` 返回的 Token 及其字面量在内部是安全的拷贝，开发者无需担心底层缓冲区的复用导致数据损坏。

## 2. StreamEncoder (流式编码)

`StreamEncoder` 将 Go 结构体直接序列化并写入 `io.Writer`。

### 2.1 性能优化
- **缓冲写入**: 使用池化的 `bufio.Writer`。
- **避免接口开销**: 通过直接调用 `WriteByte` 和 `WriteString` 避开了大量接口调用。
- **无 AST 依赖**: 直接遍历结构体并写入流。

### 2.2 使用示例
```go
enc := wanf.NewStreamEncoder(os.Stdout)
err := enc.Encode(myConfig)
```

## 3. 与标准 API 的区别

| 特性 | 标准 API (`Decode`/`Marshal`) | 流式 API (`Stream*`) |
| :--- | :--- | :--- |
| **输入/输出** | `[]byte` | `io.Reader` / `io.Writer` |
| **AST 构造** | 不构造 (即时解码) | 不构造 (流式处理) |
| **内存压力** | 适中 | 极低 |
| **适用场景** | 小规模配置、内存数据 | 大规模配置文件、高性能服务端 |
