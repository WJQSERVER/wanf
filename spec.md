### **WANF 语言规范 (草案)**

#### **1. 引言**

本文件定义了 WANF (WJQserver's Aligned Nodal Format) 语言的规范。WANF 是一种专为 Go 应用程序设计的声明式配置语言, 其核心目标是提供一种比传统格式 (如 JSON, YAML) 更直观、类型更安全且更贴近 Go 语言习惯的配置方案。

本规范的核心设计哲学是: **配置语法应尽可能地反映其映射的 Go 数据结构。** 这使得 WANF 在保持简洁的同时, 具备了上下文感知的高级特性, 能够为开发者提供自然的配置体验。

#### **2. 词法元素**

##### **2.1.** 注释 (Comments)

解析器会忽略注释。WANF 支持 Go 语言标准注释格式:

*   **单行注释**: 以 `//` 开始, 直到行尾。
*   **块注释**: 以 `/*` 开始, 以 `*/` 结束, 可以跨越多行。

```go
// main.wanf
port = 8080 // 服务监听端口

/*
 这是一个块注释,
 用于详细描述下面的配置块。
*/
log {
    level = "info"
}
```

##### **2.2.** 关键字 (Keywords)

以下标识符是保留的关键字, 不能用作配置项的键 (key): `import`, `var`。

##### **2.3.** 字面量 (Literals)

| 类型         | 格式示例                                 | 映射至 Go 类型         | 中文说明                                   |
| :----------- | :--------------------------------------- | :--------------------- | :----------------------------------------- |
| **整数**     | `value = 100`                            | `int`, `int64` 等      | 十进制整数表示。                           |
| **浮点数**   | `value = 99.5`                           | `float32`, `float64`   | 标准浮点数表示。                           |
| **布尔值**   | `value = true`                           | `bool`                 | 必须是小写的 `true` 或 `false`。           |
| **字符串**   | `value = "hello"`                        | `string`               | 由双引号或单引号包裹的单行文本。           |
| **持续时间** | `value = 5s`                             | `time.Duration`        | 由数字和时间单位 (`ns`, `us`, `ms`, `s`, `m`, `h`) 组成。 |
| **多行字符串** | `value = \`line 1\nline 2\``             | `string`               | 由反引号包裹, 保留所有内部格式和换行。     |

#### **3. 语法核心: 块、列表与分隔符**

WANF 的语法通过明确的分隔符职责来保证一致性。

##### **3.1.** 块 (Blocks)

块 (`{...}`) 用于定义一组键值对, 它们总是映射到一个 Go 的 `struct`。

*   **语法规则**: 块内的键值对**必须**由**换行符**分隔, **禁止**使用逗号作为分隔符。(常规布局下); 在紧凑布局下支持使用逗号分割
*   **设计意图**: 模拟 Go 中 `var (...)` 或 `struct{...}` 的声明形式, 强调这是一个属性的集合。
*   **Lint 警告**: 若在块内使用逗号, 解析器在 lint 模式下应发出"冗余的逗号"警告。

```go
// WANF 配置
// database 块的内容映射到单一的 DatabaseConfig 结构体。
// 因此, 其内部使用换行符分隔。
database {
    host = "localhost"
    port = 5432
}

// 非常规紧凑布局
database {host = "localhost", port = 5432}

// 即使 server 块最终成为 map 的一部分, 其块内语法也只与其
// 直接映射的 ServerConfig 结构体相关, 故同样使用换行符。
server "http_api" {
    port = 8080
    protocol = "http"
}

// 映射字面量, 同样被视为空行分隔的“块”
// Map literals are also treated as "blocks" for empty line separation.
user_roles = {[
    admin = "all_permissions",
    guest = "read_only",
]}
```

##### **3.2.** 列表 (`[...]`) 与映射 (`{[...]}`)

WANF 提供两种类似列表的结构: 用于 Go `slice` 的标准列表 (`[...]`), 以及用于 Go `map` 的映射列表 (`{[...]}`).

*   **标准列表 (`[...]`)**
    *   **用途**: 映射到 Go 的 `slice`.
    *   **分隔符**: 元素之间必须使用逗号分隔.
    *   **结尾逗号 (Trailing Comma)**: 解析器允许在最后一个元素后使用可选的尾随逗号, 但这被认为是不规范的. 官方格式化工具 (`fmt`) 会移除它, 同时 `lint` 工具会对此发出警告.

*   **映射列表 (`{[...]}`)**
    *   **用途**: 映射到 Go 的 `map`.
    *   **分隔符**: 元素之间必须使用逗号分隔.
    *   **结尾逗号 (Trailing Comma)**: 对于多行定义的映射列表, **必须**在最后一个元素后也加上逗号. 这一规则有助于版本控制和代码生成. 解析器会强制执行此规则.

```go
// WANF 配置
// services 列表, 映射到 []string.
// 结尾逗号是不推荐的, lint会发出警告.
services = [
    "auth",
    "payment",
] // 推荐格式

```

##### **3.3** 语句分隔符 (Statement Separators)
WANF 中的语句 (如赋值语句 `key = value` 或块语句 `block { ... }`) 必须由明确的分隔符进行分割。

*   换行符 (`\n`): 这是标准且推荐的分隔符, 用于常规布局, 可以提供最佳的可读性。

*   分号 (`;`): 在需要将多个语句写在同一行(紧凑布局)时, 可以使用分号作为分隔符。它的作用与换行符完全等效。

示例:

```wanf
// 标准的多行布局
host = "localhost"
port = 8080
// 等效的单行紧凑布局
host = "localhost"; port = 8080
```
注意:

在块 (`{...}`) 内部, 语句同样遵循此规则, 即在紧凑布局下可以使用分号分隔。
在列表 (`[...]`) 内部, 元素必须使用逗号 (`,`) 分隔, 不能使用分号。

#### **4. 高级功能**

##### **4.1.** 变量 (`var`)

`var` 用于在文件顶部声明变量, 其作用域仅限于当前文件。

*   **声明**: `var identifier = value`
*   **引用**: `${identifier}`
*   **流式解码器限制**: 为了实现最高的性能和最低的内存占用，`StreamDecoder`（流式解码器）**不支持** `var` 语句。如果在流式模式下遇到 `var` 声明，解码器将报告一个错误。

```go
// WANF 配置
var default_protocol = "http"

server "main" {
    port = 8080
    protocol = "${default_protocol}"
}
```

##### **4.2.** 环境变量 (`env`)

`env()` 函数用于从系统环境变量中读取值。

*   `env("VAR_NAME")`: 若环境变量 `VAR_NAME` 未设置, 解析将失败并返回错误。
*   `env("VAR_NAME", "default_value")`: 若未设置, 则使用提供的字符串字面量作为默认值。

```go
// WANF 配置
database {
    // 推荐为敏感信息使用此方式
    password = env("DB_PASSWORD")
    // 为可选项提供默认值
    host = env("DB_HOST", "localhost")
}
```

##### **4.3.** 文件导入 (`import`)

`import` 指令用于将配置文件模块化。

*   **声明**: `import "path/to/another.wanf"`
*   **路径规则**: 路径是相对于当前文件的。
*   **作用域规则**: 被导入文件中的变量不会污染导入它的文件。
*   **流式解码器限制**: 为了实现最高的性能和最低的内存占用，`StreamDecoder`（流式解码器）**不支持** `import` 语句。如果在流式模式下遇到 `import` 声明，解码器将报告一个错误。

```go
// main.wanf
import "./shared/database.wanf"

// 多个 server 声明通过换行符在顶层分隔
server "app_a" {
    port = 8080
}

server "app_b" {
    port = 8081
}

// shared/database.wanf
database {
    host = "localhost"
    user = "admin"
}
```

#### **5.** 核心映射规则: `wanf` 结构体标签

WANF 解析器通过 Go 结构体字段的 `wanf` 标签来确定映射关系。

*   **基础映射**: `wanf:"name"` 将字段与 WANF 文件中名为 `name` 的键或块进行关联。

* **忽略空值 (omitempty)**: `wanf:"name,omitempty"`
    当字段值为其类型的零值 (如空字符串、0、`nil` 指针) 或长度为 0 的集合 (如空 `slice` 或 `map`) 时, 编码器将在输出中忽略该字段。
    注意: 出于兼容性考虑, 即使未显式设置 `omitempty`, 长度为 0 的 `map` 也会被忽略。

*   **列表到 Map 的映射**: `wanf:"services,key=name"`
    此标签指示解析器:
    1.  找到名为 `services` 的列表。
    2.  列表中的每个对象都是一个 `Service` 结构体。
    3.  使用每个 `Service` 结构体中 `name` 字段的值作为最终 Go `map` 的键。

```wanf
// WANF 配置
// 列表将被转换为 map
components = {[
    "1" {
        id = "comp_a"
        priority = 10
    },
    "2" {
        id = "comp_b"
        priority = 20
    },
]}
```

```go
// Go 结构体定义
type Component struct {
    ID       string `wanf:"id"`
    Priority int    `wanf:"priority"`
}

type RootConfig struct {
    // "components" 列表将根据 "id" 字段的值被解析为一个 map
    Components map[string]Component `wanf:"components,key=id"`
}
```

```wanf
dashMap {[
	key1 = "value1",
	key2 = "value2",
]}
```

```go
DashMap  map[string]string   `wanf:"dashMap"`

		DashMap: map[string]string{
			"key1": "value1",
			"key2": "value2",
		},
```

```wanf
sList {[
	item1 = {}
	item2 = {}
]}
```

```go
	SList    map[string]struct{} `wanf:"sList"`

		SList: map[string]struct{}{
			"item1": {},
			"item2": {},
		},
```

#### **6.** 错误处理与 Lint 模式

*   **解析错误 (Fatal Errors)**: 这类错误将立即中止解析过程。
    *   **示例**: 无效的语法 (如在块内使用逗号)、引用了不存在的变量、调用 `env()` 时所需的环境变量未设置且无默认值。

*   **Lint 警告 (Non-Fatal Warnings)**: 这类问题不影响配置的成功解析, 但表示存在不规范或可能与开发者意图不符的写法。解析器工具应提供一个可选的 `lint` 模式来启用这些检查。
    *   **示例**:
        *   为一个将映射到单一结构体的块提供了名称 (如 `log "main" { ... }`)。
        *   在块内使用了逗号 (即使解析器可能容忍它)。

#### **7.** 规则总结

##### **块 (Block) 的映射**

| Go 结构体字段类型 | WANF 块示例 (注意分隔符) | 语法要求 |
| :--- | :--- | :--- |
| **单一结构体** `T` | `log { level = "info" }` | **块内**必须使用**换行符**分隔。 |
| **单一结构体** `T` | `log "main" { ... }` | **不规范**: 名称 `"main"` 被忽略。Lint 模式应报告此问题。 |
| **Map** `map[string]T` | `server "http" { port = 8080 }` | **块内**必须使用**换行符**。多个 `server` 块在顶层由换行符分隔。 |

##### **列表 (List) 的灵活解析**

| Go 结构体字段类型 | WANF 列表示例 (`[]`内) | 语法要求 |
| :--- | :--- | :--- |
| **切片** `[]string` | `"a", "b", "c",` | **列表内**必须使用**逗号**分隔元素。 |
| **Map (基于字段值)** `map[string]T` | `{id="a"}, {id="b"},` | **列表内**必须使用**逗号**分隔元素。需配合 `wanf:",key=..."` 标签。 |
| **Set** `map[string]struct{}` | `"feature_a", "feature_b",` | **列表内**必须使用**逗号**分隔元素。 |

#### **8. 编码器输出格式 (Encoder Output Formatting)**

官方的 wanf 编码器提供多种输出样式, 以满足不同场景下的需求, 从人类可读到机器优化。

##### **8.1.** 默认样式 (StyleBlockSorted)

这是编码器的默认行为, 旨在提供最佳的可读性和结构清晰度。(为了向后兼容, `StyleDefault` 是此样式的别名).

**排序规则**:
*   **顶层字段**: 严格按照 Go 结构体中的定义顺序输出, 以保留开发者设计的逻辑章节顺序。
*   **嵌套块内部**: 对字段进行排序。简单键值对 (kv) 会排在嵌套块 (block) 之前, 同类型的字段会按名称的字母顺序排序。
**格式**:
*   使用制表符 (`\t`) 进行缩进。
*   在顶级的块 (`{...}`), 映射 (`{[...]}`) 或列表 (`[...]`) 之间, 以及它们与顶级键值对之间, 会使用一个空行来分隔, 以增强可读性。
**示例**:

```
# Go Struct: { C_kv, A_block, B_kv }
# Output:
c_kv = "c"

a_block {
    # 内部排序: a_sub_kv 在 b_sub_kv 之前
    a_sub_kv = "a"
    b_sub_kv = "b"
}

b_kv = 123
```
##### **8.2.** 全局排序样式 (StyleAllSorted)

此样式用于生成一个完全规范化、不受 Go 结构体定义顺序影响的配置文件。

**排序规则**: 在所有层级 (包括顶层) 都应用排序规则: 键值对在前, 块在后, 同类按字母顺序。
**格式**: 默认带空行, 但可以与 `WithoutEmptyLines()` 选项组合使用以生成紧凑的排序输出。
**示例**:

```
# Go Struct: { C_kv, A_block, B_kv }
# Output:
# kv 在前并按字母排序
b_kv = 123
c_kv = "c"

# block 在后
a_block {
    a_sub_kv = "a"
    b_sub_kv = "b"
}
```
##### **8.3.** 流式样式 (StyleStreaming)

此样式为性能而生, 不进行任何排序。

**排序规则**: 无排序。所有字段严格按照 Go 结构体中的定义顺序输出。
**格式**: 使用制表符缩进, 但块之间没有额外的空行。

##### **8.4.** 单行样式 (StyleSingleLine)

此样式用于生成最紧凑的机器可读格式。

**排序规则**: 无排序。
**格式**:
*   无任何换行和缩进。
*   使用分号 (`;`) 作为语句分隔符。
**示例**:

```
c_kv="c";a_block{b_sub_kv="b";a_sub_kv="a"};b_kv=123
```