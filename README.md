# WANF - WJQserver's Aligned Nodal Format

**WANF** 是一种富有表现力、无歧义的声明式配置语言。其核心设计哲学是：**配置语法应尽可能地反映其映射的数据结构**。

这使得WANF在保持简洁、人类可读的同时，具备了上下文感知的高级特性，能够为开发者提供符合直觉的配置体验，有效减少因配置错误引发的问题。

[![Go Reference](https://pkg.go.dev/badge/wanf.svg)](https://pkg.go.dev/github.com/WJQSERVER/wanf)
[![Go Report Card](https://goreportcard.com/badge/github.com/WJQSERVER/wanf)](https://goreportcard.com/report/github.com/WJQSERVER/wanf)

---

## 核心特性与优势

### 1. 清晰的结构与边界

WANF 使用显式的 `{}` 作为块的边界，彻底解决了类似 YAML 中依赖缩进的痛点。这使得重构和复制/粘贴配置块变得安全、简单，不再担心因错误的缩进级别导致难以察觉的 bug。

```wanf
// ✅ 清晰、无歧义的结构
server {
    host = "localhost"
    port = 8080
}

database {
    host = "127.0.0.1"
}
```

### 2. 专为版本控制设计的列表与映射语法

WANF 对用于 `slice` 的列表 (`[...]`) 和用于 `map` 的映射 (`{[...]}`) 采用了不同的、更严格的语法，这在团队协作和代码审查中极具优势。

*   **列表 (`[...]`)**: 元素由逗号分隔。
*   **映射 (`{[...]}`)**: 条目同样由逗号分隔，且**强制要求多行映射的最后一个条目后也必须有尾随逗号**。

这个尾随逗号的规则使得在版本控制系统 (如 Git) 中添加或删除条目时，diff 会非常干净，只显示被修改的行，而不会因为逗号的添加或删除而污染相邻行。

```wanf
// services 列表，映射到 Go 的 []string
services = [
    "auth",
    "payment", // 尾随逗号是可选的，但推荐
]

// user_roles 映射，映射到 Go 的 map[string]string
// 注意最后一个条目 "guest" 后面的逗号是必需的！
user_roles = {[
    admin = "all_permissions",
    guest = "read_only", // <-- 这个逗号让 git diff 更清晰
]}
```

### 3. Go 语言原生体验

WANF 的语法设计与 Go 的复合字面量 (composite literals) 非常相似，对 Go 开发者极为友好。它还原生支持 Go 标准库中的 `time.Duration` 类型。

```wanf
// Go 代码:
// cfg := struct{ Timeout time.Duration }{ Timeout: 10 * time.Second }

// WANF 配置:
timeout = 10s // 直观且类型安全
```

### 4. 丰富的字面量支持

WANF 支持所有常见的字面量类型：

| 类型 | 格式示例 |
| :--- | :--- |
| **整数** | `value = 100` |
| **浮点数** | `value = 99.5` |
| **布尔值** | `value = true` |
| **字符串** | `value = "hello"` |
| **多行字符串** | `value = \`line 1\nline 2\`` |
| **持续时间** | `value = 5m30s` |

## `wanflint`: 官方工具链

`wanflint` 是 WANF 的官方 Linter 和格式化工具，是保证代码质量和一致性的利器。

### 安装

```sh
go install github.com/WJQSERVER/wanf/wanflint@latest
```

### `wanflint fmt` - 智能格式化

`fmt` 命令可以自动将您的 `.wanf` 文件格式化为统一、整洁的风格。

**核心特性**:
*   **智能注释保留**: 与许多格式化工具不同，`wanflint fmt` 能够完美保留您的所有注释，包括行尾注释和独立的块注释，确保代码在格式化后依然保持高度的可读性。
*   **规范化排序**: 默认情况下，`fmt` 会对嵌套块内的字段按字母顺序进行排序（键值对在前，嵌套块在后），这有助于保持配置文件的确定性和规范性。
*   **灵活的排序控制**:
    *   `--nosort`: 如果您希望保持字段的原始书写顺序（例如，为了逻辑上的分组），可以使用此标志禁用自动排序。格式化工具将只调整缩进和间距，而完全尊重您的原始顺序。
    *   `-d`: 将格式化后的结果输出到标准输出，而不是直接修改文件。

**使用示例**:
```sh
# 格式化文件 (默认排序)
wanflint fmt your_config.wanf

# 格式化文件并禁用排序
wanflint fmt --nosort your_config.wanf
```

### `wanflint lint` - 全方位代码检查

`lint` 命令不仅检查语法错误，还会报告风格问题和潜在的逻辑错误。

**检查范围**:
*   **语法错误**: 报告所有致命的解析错误，例如无效的标记或错误的结构。
*   **风格警告**: 提示不符合最佳实践的写法，例如：
    *   `ErrRedundantComma`: 在 `{}` 块中使用了不必要的分号。
    *   `ErrRedundantLabel`: 为非 map 类型的块提供了多余的标签。
*   **逻辑问题**: 发现潜在的运行时问题，例如：
    *   `ErrUnusedVariable`: 声明了但从未使用的 `var` 变量。
*   **机器可读输出**:
    *   `--json`: 以 JSON 格式输出所有错误和警告，方便与 VSCode 等编辑器或 CI/CD 工具链进行深度集成。

**使用示例**:
```sh
# 检查文件
wanflint lint your_config.wanf

# 以 JSON 格式输出检查结果
wanflint lint --json your_config.wanf
```

## Go 语言集成

在您的 Go 应用中使用 WANF 非常简单。

#### 1. 定义你的 Go 结构体
使用 `wanf` 标签来映射配置文件中的键。

```go
package main

import (
    "fmt"
    "github.com/WJQSERVER/wanf"
    "time"
)

type Config struct {
    Server   map[string]ServerConfig `wanf:"server"`
    Database DatabaseConfig        `wanf:"database"`
    Services []string                `wanf:"services"`
}

type ServerConfig struct {
    Host       string        `wanf:"host"`
    Port       int           `wanf:"port"`
    MaxStreams int           `wanf:"max_streams,omitempty"` // 忽略零值
}

type DatabaseConfig struct {
    User     string `wanf:"user"`
    Password string `wanf:"password"`
}
```

#### 2. 解析配置文件

```go
func main() {
    var cfg Config
    // 假设这是你的 your_config.wanf 文件内容
    data := []byte(`
        server "main_api" {
            host = "0.0.0.0"
            port = 8080
        }
        database {
            user = "admin"
            password = "password"
        }
        services = ["auth", "payment"]
    `)

    err := wanf.Decode(data, &cfg)
    if err != nil {
        panic(err)
    }

    fmt.Printf("Parsed config: %+v\n", cfg)
}
```

## 高级功能

### 变量 (`var`)
`var` 用于在文件顶部声明变量，其作用域仅限于当前文件。

*   **声明**: `var identifier = value`
*   **引用**: `${identifier}`

```wanf
var default_protocol = "http"

server "main" {
    protocol = "${default_protocol}"
}
```

### 环境变量 (`env`)
`env()` 函数用于从系统环境变量中读取值，是管理敏感信息的推荐方式。

*   `env("VAR_NAME")`: 若环境变量 `VAR_NAME` 未设置，解析将失败。
*   `env("VAR_NAME", "default_value")`: 若未设置，则使用提供的默认值。

```wanf
database {
    password = env("DB_PASSWORD")
    host = env("DB_HOST", "localhost")
}
```

### 文件导入 (`import`)
`import` 指令用于将配置文件模块化，但请注意，被导入文件中的变量不会污染导入它的文件。

*   **声明**: `import "path/to/another.wanf"`

## 编辑器集成

为了获得最佳的开发体验, 建议安装官方的VS Code扩展, 它提供了语法高亮、实时`lint`检查和格式化功能.

*   **[WANF Language Support on VS Code Marketplace](https://marketplace.visualstudio.com/items?itemName=wjqserver.wanf-language-support)**

## 详细文档

有关内部实现、标签使用和流式 API 的更多信息，请参阅 `docs` 目录：

- [内部实现与优化](./docs/internal.md)
- [Neo 极速编解码器](./docs/neo.md)
- [流式 API](./docs/streaming.md)
- [结构体标签 (wanf tag)](./docs/tags.md)
- [编码格式详解](./docs/encoding_formats.md)

## 贡献

本项目欢迎各种形式的贡献，包括但不限于：
*   Bug 报告和修复
*   功能建议
*   文档改进

在您贡献代码前，请先创建一个 Issue 来讨论您的想法。
