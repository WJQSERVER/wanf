package wanf

import (
	"reflect"
	"time"
	"unsafe"
)

var (
	durationType        = reflect.TypeFor[time.Duration]()
	mapStringAnyType    = reflect.TypeFor[map[string]any]()
	mapStringStringType = reflect.TypeFor[map[string]string]()
)

// StringToBytes 将字符串转换为字节切片, 不进行内存分配.
// 更多详情, 请参见 https://github.com/golang/go/issues/53003#issuecomment-1140276077.
// 注意: 此函数使用 unsafe 包, 应谨慎使用, 因为它可能导致内存不安全.
// 返回的 []byte 切片绝不能被修改.
func StringToBytes(s string) []byte {
	return unsafe.Slice(unsafe.StringData(s), len(s))
}

// BytesToString 将字节切片转换为字符串, 不进行内存分配.
// 更多详情, 请参见 https://github.com/golang/go/issues/53003#issuecomment-1140276077.
// 注意: 此函数使用 unsafe 包, 应谨慎使用, 因为它可能导致内存不安全.
// 在 b 被修改后, 返回的字符串不应再被使用.
func BytesToString(b []byte) string {
	return unsafe.String(unsafe.SliceData(b), len(b))
}
