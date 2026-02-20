> [更多详细编码格式说明请参考 encoding_formats.md](./encoding_formats.md)

# WANF 结构体标签 (Struct Tags)

WANF 使用 `wanf` 标签来控制 Go 结构体字段与配置文件之间的映射。

## 1. 基础映射

最简单的用法是指定配置文件中的键名。

```go
type Config struct {
    Host string `wanf:"server_host"`
}
```

## 2. omitempty (忽略空值)

`omitempty` 选项允许在字段值为零值或空集合时，在编码输出中省略该字段。

### 2.1 支持的零值类型
- **数值**: `0` (int, float 等)
- **字符串**: `""`
- **布尔值**: `false`
- **指针**: `nil`
- **切片 (Slice)**: 长度为 0 的切片（包括 `nil` 切片和 `[]T{}`）。

### 2.2 特殊情况：Map
出于向后兼容和 WANF 设计规范的考虑，**长度为 0 的 `map` 无论是否标记 `omitempty` 都会在输出中被忽略**。

```go
type Config struct {
    // 如果 String 为 ""，则不输出
    String string `wanf:"string,omitempty"`

    // 如果 Slice 长度为 0，则不输出
    List []string `wanf:"list,omitempty"`
}
```

## 3. key (列表转 Map)

当 WANF 配置文件中使用列表表示一组对象，而你希望在 Go 中将其解析为 `map` 时，可以使用 `key` 选项。

### 3.1 语法
`wanf:"<key_in_config>,key=<field_to_use_as_map_key>"`

### 3.2 示例
**WANF 配置:**
```wanf
users = [
    { id = "alice", age = 30 },
    { id = "bob", age = 25 }
]
```

**Go 结构体:**
```go
type User struct {
    ID  string `wanf:"id"`
    Age int    `wanf:"age"`
}

type Config struct {
    // 将 users 列表解析为以 User.ID 为键的 map
    Users map[string]User `wanf:"users,key=id"`
}
```

## 4. 块 (Blocks) vs 值 (Values)

WANF 根据 Go 类型自动判断映射形式：
- **结构体 (Struct)**: 映射为块 `name { ... }`。
- **其他类型 (Map, Slice, Basic types)**: 映射为赋值 `name = ...`。
- **特殊类型**: `time.Duration` 被视为基本值类型。
