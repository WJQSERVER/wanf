# WANF 编码格式详解

本文档详细描述了 Go 语言中的各种类型在经过 WANF 编码器（Encoder）处理后生成的格式。

## 1. 基本类型 (Basic Types)

| Go 类型 | WANF 格式 | 示例 | 说明 |
| :--- | :--- | :--- | :--- |
| **整数** (`int*`, `uint*`) | 十进制数字 | `123`, `456` | 使用 `strconv.AppendInt` 或 `strconv.AppendUint` 生成。 |
| **浮点数** (`float*`) | 浮点数字 | `99.5`, `1.234` | 使用 `strconv.AppendFloat` (格式 'f', 精度 -1)。 |
| **布尔值** (`bool`) | `true` 或 `false` | `true` | 仅支持小写。 |
| **持续时间** (`time.Duration`) | 字符串序列 | `5s`, `1h30m` | 调用 `.String()` 方法生成。 |

## 2. 字符串 (Strings)

WANF 根据内容自动选择字符串格式：

- **单行/通用**: 使用双引号 `"` 包裹。
  - 特殊字符（如 `\n`, `\r`, `\t`, `"`, `\`）会被转义。
  - 示例: `"hello \"world\""`
- **多行**: 如果字符串包含换行符且编码样式不是 `StyleSingleLine`，则使用反引号 `` ` `` 包裹。
  - 内容按原样保留，不进行转义。
  - 示例:
    ```wanf
    key = `line 1
    line 2`
    ```

## 3. 结构体 (Structs)

结构体映射为 WANF 中的“块”。

- **格式**: `name { ... }`
- **字段分隔**: 在块内部，字段通常由换行符分隔。
- **空结构体**: 编码为 `name {}`。
- **字段顺序**: 默认情况下按 Go 结构体定义的顺序输出，但可以通过选项启用排序。

## 4. 映射 (Maps)

映射在 WANF 中有特殊的语法，用于区分于结构体块。

- **格式**: `name {[ ... ]}`
- **键值分隔**: 键与值之间使用 `=` 分隔。
- **条目分隔**: 条目之间使用逗号 `,` 分隔。
- **排序**: 为了保证输出的确定性，Map 的键始终按字母顺序排序。
- **特殊处理**: **空 Map 在结构体中默认会被忽略**（即使没有 `omitempty` 标签）。

## 5. 切片与数组 (Slices & Arrays)

- **格式**: `name = [ ... ]`
- **元素分隔**: 元素之间使用逗号 `,` 分隔。
- **空集合**: 编码为 `[]`。如果设置了 `omitempty` 标签且切片长度为 0，则该字段会被完全忽略。

## 6. 指针与接口 (Pointers & Interfaces)

- **指针**: 自动解引用。如果指针为 `nil`，则忽略该字段（对于结构体字段）或不输出值。
- **接口**: 自动获取接口内部的实际值进行编码。如果接口为 `nil`，则不输出。

## 7. 复杂嵌套示例

```go
type Config struct {
    ID      int               `wanf:"id"`
    Tags    []string          `wanf:"tags"`
    Meta    map[string]string `wanf:"meta"`
    Details struct {
        Active bool `wanf:"active"`
    } `wanf:"details"`
}
```

**编码后的 WANF:**
```wanf
id = 1
tags = ["go", "wanf"]

meta {[
    env = "prod",
    region = "us-east",
]}

details {
    active = true
}
```
