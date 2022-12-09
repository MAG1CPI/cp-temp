# 此为临时的编译原理合作仓库

## 类型检查&中间代码生成-更新进度

### CHECK

- [x] 变量/常量/函数重复定义
- [ ] 函数参数检查
- [ ] 函数重载
- [x] 变量/常量/函数未定义
- [x] 函数当作变量使用
- [x] 变量当作函数使用
- [x] `break` 和 `continue` 在 `while` 外使用
- [x] `return` 在函数外
- [x] `return` 不恰当的返回类型(non-void vs void)
- [x] `void` 函数作为操作数
- [x] 不合法的函数参数类型 `void`

### 接口速查

* `type` 
  * `IntType`
  * `ConstIntType`
  * `FloatType`
  * `ConstFloatType`
  * `VoidType`
  * `FunctionType`
  * `PointType`
