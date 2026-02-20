# WANF 内部实现与优化

本文档详细介绍了 WANF 库的内部实现机制及为了追求极致性能而采用的各种优化手段。

## 1. 零分配词法分析器 (Zero-Allocation Lexer)

WANF 的 `Lexer` 经过精心设计，在对输入的 `[]byte` 进行分词时实现了 **0 分配**。

- **切片重用**: `Lexer` 直接在原始输入切片上操作，返回的 `Token` 字面量是原始数据的子切片。
- **避免接口封箱**: 移除了早期的 Lexer 池化逻辑，因为它引入了接口封箱开销，反而降低了性能。
- **查找表优化**: 使用预计算的 `isIdentTable` 查找表快速识别 ASCII 标识符字符。

## 2. 即时解码 (On-the-fly Decoding)

为了显著降低内存占用和减少分配，WANF 在 `Decode` 和 `StreamDecoder` 中弃用了传统的抽象语法树 (AST) 构造过程，改用“即时解码”模式。

- **跳过 AST**: Token 流直接映射到 Go 结构体，无需中间的 Node 节点分配。
- **分配降低**: 相比于原始基于 AST 的解析器，分配次数减少了约 92%（标准 `Decode` 从 ~173 次分配降至 ~14 次）。

## 3. 反射优化与缓存

反射（Reflection）通常是 Go 序列化库的性能瓶颈。WANF 通过以下手段实现了“反射less”（反射最小化）优化：

### 3.1 字段信息缓存 (`fieldCache`)
使用 `sync.Map` 缓存结构体的元数据（`cachedStructInfo`），包括：
- **预排序字段**: 根据编码样式预先排好序的字段列表。
- **名称预转字节流**: 提前将字段名转换为 `[]byte` (`nameBytes`)。
- **类型信息缓存**: 缓存字段的 `reflect.Kind` 和 `isCollection` 标志，在编码循环中避免重复调用 `fv.Kind()`。

### 3.2 类型切换快路径 (Type-Switch Fast-paths)
在 `encodeInterface` 和 `decodeValueTo` 等关键路径中，大量使用 `switch` 匹配常用类型（`int`, `string`, `bool`, `time.Duration`, `map[string]any` 等），避开通用的反射逻辑。

## 4. 资源池化

WANF 广泛使用 `sync.Pool` 来重用临时对象，以减轻垃圾回收（GC）压力：
- **内部编码器/解码器**: 重用 `internalEncoder` 和 `internalDecoder` 实例。
- **缓冲区**: 重用 `bufio.Writer` 和字节切片。
- **排序辅助**: 重用用于 Map 键排序的字符串切片和 `mapEntry` 切片。

## 5. 零拷贝转换

在处理内部逻辑（如字段名查找）时，使用 `unsafe` 进行 `[]byte` 和 `string` 的零拷贝转换（见 `utils.go` 中的 `BytesToString` 和 `StringToBytes`），进一步压榨性能。

## 6. 测试与持续回归检查

为了防止类似指针解引用崩溃等问题的回归，项目在 `test/` 目录下维护了一系列针对已知历史问题的回归测试：
- `repro_panic_test.go`: 检查指针集合（Slice, Map）的 `omitempty` 逻辑。
- `repro_struct_ptr_panic_test.go`: 检查嵌套结构体指针的编码安全性。
