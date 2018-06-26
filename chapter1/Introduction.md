# 第一章 各就各位，预备！

Haskell 中的负数需要用 `()`：

```Haskell
5 * 2 * (-10)
```

## 函数调用

`*` 即为函数，但它是中缀的，称之为 **中缀函数**（infix function）。

大多数函数为 **前缀函数**：

```Haskell
succ 8
min 10 12
max 10 102
```

Haskell 中 **函数调用** 优先级最高：

```Haskell
succ 9 + max 5 4 + 1
```

等价于：

```Haskell
(succ 9) + (max 5 4) + 1
```

结果为 16。

而：

```Haskell
succ 9 * 10
```

等价于：

```Haskell
(succ 9) * 10
```

若某函数有两个参数，可以用反引号 ` 将其包裹，以中缀形式调用：

```Haskell
div 100 4
```

等价于：

```Haskell
100 `div` 4
```

## 函数定义

一个参数：

```Haskell
doubleMe x = x + x
```

* `+` 函数可用于整数、浮点数

两个参数：

```Haskell
doubleUs x y = x * 2 + y * 2
```

>Haskell 中函数定义没有 **顺序** 的概念，谁先谁后没有影响。

`doubleUs` 可以基于 `doubleMe` 定义：

```Haskell
doubleUs x y = doubleMe x + doubleMe y
```

if-then-else：

```Haskell
doubleSmallNumber x = if x > 100
                      then x
                      else x * 2
```

* `else` 不可省略

Haskell 中的 if-then-else 是表达式，而非语句：

```Haskell
doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1
```

`'` 是合法的函数名字字符：

```Haskell
song'kun = "it's me Song Kun"
```

* 函数只能以 **小写** 字母开头
* 没有参数的函数，也叫定义 or 名字

## 列表入门

```Haskell
lostNumbers = [1, 2, 3, 4]
```

* 元素类型相同

### 列表拼接

```Haskell
[1, 2, 3] ++ [4, 5, 6]
"Hello" ++ "world"
['h', 'e', 'l', 'l', 'o'] ++ ['w', 'o', 'r', 'l', 'd']
```

>Haskell 字符串实际是 `[Char]` 的语法糖。

`++` 会遍历左边的列表，因此列表较长时，需要注意效率。

头部插入：

```Haskell
'A' : "Hello"
1 : [2, 3]
```

* 成本几乎为 0

注意：

```Haskell
[1, 2, 3]
```

是

```Haskell
1 : 2 : 3 []
```

的语法糖。

### 访问列表元素

按索引：

```Haskell
[1, 2, 3] !! 2
```

* 索引从 0 开始
* 小心越界错误

### 嵌套列表

```Haskell
[[1, 2, 3], [4, 5, 6]]
```

嵌套的列表长度可以不同，但类型必须相同。

### 比较列表

只要 **列表元素** 可以比较大小，就可用 `>`、`<`、`>=` 等比较列表大小，规则如下：

* 比较两个列表第一个元素，若相等，比较第二个 ...
* 直到遇到不相等的元素，列表大小以该元素比较为准

```Haskell
[1, 2, 3] < [2]
```

>非空列表总是比 `[]` 大。

### 更多列表操作

`head`：

```Haskell
head [1, 2, 3]  -- 1
```

`tail`：

```Haskell
tail [1, 2, 3]  -- [2, 3]
```

`last`：

```Haskell
last [1, 2, 3]  -- 3
```

`init`：

```Haskell
init [1, 2, 3]  -- [1, 2]
```

这 4 个函数用于 `[]` 都会报错，且编译时无法发现。

`null` 检查列表是否为空：

```Haskell
null [1, 2, 3]  -- False
null []  -- True
```

`reverse` 翻转列表：

```Haskell
reverse [1, 2, 3]  -- [3, 2, 1]
```

`take`：

```Haskell
take 3 [1, 2, 3, 4]  -- [1, 2, 3]
```

若 `n` 超过列表长度，会获取整个列表：

```Haskell
take 100 [1, 2, 3, 4]  -- [1, 2, 3, 4]
```

`drop` 与 `take` 相反：

```Haskell
drop 3 [1, 2, 3, 4] -- [4]
drop 0 [1, 2, 3, 4] -- [1, 2, 3, 4]
drop 100 [1, 2, 3, 4]  -- []
```

最大、最小元素（元素必须可比较）：

```Haskell
maximum [1, 2, 3, 4]  -- 4
minimum [1, 2, 3, 4]  -- 1
```

所有元素之和、之积：

```Haskell
sum [1, 2, 3, 4]  -- 10
product [1, 2, 3, 4]  -- 24
```

是否包含某元素：

```Haskell
1 `elem` [1, 2, 3]  -- True
```

* 常用中缀形式调用

## 区间

区间（range）是构造列表的方式之一，区间中的元素必须是可排序的:

```Haskell
[1 .. 20]
['a' ..  'z']
```

获取 1-20 之间所有偶数：

```Haskell
-- [2,4,6,8,10,12,14,16,18,20]
[2, 4 .. 20]
```

* 列出前两个元素 `[2, 4]`，加上区间上界 `20` 即可
* 通过列出前两个元素，可以指定 **步长**

1-20 间 3 的倍数：

```Haskell
[3, 6 .. 20]
-- [3,6,9,12,15,18]
```
