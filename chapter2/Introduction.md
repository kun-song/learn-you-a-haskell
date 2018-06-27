# 第二章 相信类型

Haskell 中一切皆有类型，且表达式的类型在 **编译时** 确定，在运行时 **擦除**。

Haskell 支持类型推导，从而获取类似动态语言的灵活性。

## 声明类型

查看表达式类型：

```Haskell
ghci>:t 1 + 2
```

类型的首字母必须 **大写**，例如：`(Bool, Char, Int)`。

类型用 `::` 声明，读作“它的类型是”：

```Haskell
removeNoUppercase :: [Char] -> [Char]
removeNoUppercase s = [x | x <- s, x `elem` ['A' .. 'Z']]
```

## 常见类型

整型：

* `Int` 为机器相关整型，范围与机器有关，例如 -2^63 - 2 ^63 - 1，`Int` 一定有界；
* `Integer` 也是整型，但它是 **无界** 的，只要内存放得下就行，但效率不如 `Int`；

```Haskell
fact :: Integer -> Integer
fact n = product [1 .. n]

fact 100
-- 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000
```

浮点数：

* `Float`
* `Double`

```Haskell
circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r

λ> circumference 4
25.132742
λ> circumference' 4
25.132741228718345
```

* 注意两者精度区别

## 类型变量

`head`、`fst` 类型：

```Haskell
λ> :t head
head :: [a] -> a
λ> :t fst
fst :: (a, b) -> a
```

类型首字母必须大写，这里的 `a`、`b` 都是类型参数，参数多态，`head` 为多态函数。

## type class 入门

type class 是定义 **行为** 的接口，是 **一组函数** 的集合。

若某类型（type）是 type class 的实例，则表明该 type 必然实现了 type class 规定的函数。

看 `(==)` 函数的类型：

```Haskell
λ> :t (==)
(==) :: Eq a => a -> a -> Bool
```

`=>` 左侧的 `Eq a` 为类型约束（type constraint），表明类型 `a` 是 `Eq` type class 的实例！

>注意：若函数名字全部是特殊字符，如 `==`，则默认为中缀函数，当：
>* 用 `:t` 检查其类型
>* 传递给其他函数
>* 作为前缀函数使用
>时，必须加括号：`(==)`。

在 Haskell 中，凡是可比较相等性的类型都是 `Eq` type class 的实例。

### `Eq`

`Eq` type class 定义了两个函数：

* `==`
* `/=`

`Eq` 的实例必须提供这两个函数的实现。

```Haskell
1 == 2
'a' == 'a'
"Hello" == "world"
```

Haskell 的标准类型（除 IO 相关）都是 `Eq` type class 的实例，因此都可以使用 `==` 和 `/=`。
