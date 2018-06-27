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

If a type is a part of a typeclass（typeclass is a group of classes which obey some behaviour），则表明 the type of those two values must be a member of the `Eq` class。

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

在 Haskell 中，凡是可比较相等性的类型都是 a member of the `Eq` class。

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

## `Ord`

`Ord` is for types that have an ordering.

```Haskell
:t (>)
-- (>) :: Ord a => a -> a -> Bool
```

目前除函数类型外，其他类型都是 are part of `Ord`。

`Ord` 定义了 `>`、`>=`、`<`、`<=` 以及 `compare` 等函数，其中 `compare` 接受两个 `Ord`，产生一个 `Ordering`。

`Ordering` 有 3 个值：

* `LT`
* `GT`
* `EQ`

一个类型（type）要属于 `Ord`，首先要属于 `Eq`。

```Haskell
   "Song" > "Kun"
=> True
   "Song" `compare` "Kun"
=> GT
   5 < 10
=> True
   5 `compare` 10
=> LT
```

### `Show`

Members of `Show` can be presented as Strings.

All types convered so far except for functions are part of `Show`.

`Show` 定义了 `show` 函数，接受一个值（whose type is a member of `Show`），并将该值转换为 string：

```Haskell
   show 5
=> "5"
   show True
=> "True"
```

### `Read`

`Read` 某种意义上作用与 `Show` 相反，`read` function takesa string, and returns a type which is a member of `Read`.

```Haskell
   read "[1, 2, 3]" ++ [4]
=> [1,2,3,4]
   read "True" && False
=> False
   read "100" * 2.4
=> 240.0
```

单独使用会报错：

```Haskell
read "[1, 2, 3]"
```

因为 `read` 将输入字符串转换为 `Read` type class 中的某个类型，但是单独用时编译器不知道应该转换为哪个具体类型，所以报错误，而前面根据上下文，编译器能够自动推断出应该输出的类型。

也可以显式声明 `read` 的返回类型：

```Haskell
read "[1, 2, 3]" :: [Int]
read "(\"Hello\", 100)" :: (String, Int)
```

这样就不会报错了。

### `Enum`

`Enum` members are sequentially ordered types -- they can be enumerated. 

* The main advantage of `Enum` type class is that we can use its types in list ranges.
* `Enum` 有前驱、后继，分别用 `succ` 和 `pred` 获取

属于 `Enum` type class 的类型有：

* `()`
* `Int` `Integer` `Double` `Float`
* `Bool`
* `Char`
* `Ordering`

```Haskell
ghci> [LT .. GT]  
[LT,EQ,GT]
ghci> succ 'B'  
'C'
```

### `Bounded`

`Bounded` members have an upper and a lower bound.

`minBound` 和 `maxBound` 获取上下界：

```Haskell
ghci> minBound :: Int  
-2147483648  
ghci> maxBound :: Char  
'\1114111'  
ghci> maxBound :: Bool  
True  
ghci> minBound :: Bool  
False  
```

如果 tuple 中所有元素类型属于 `Bounded`，则整个 tuple 也属于 `Bounded`：

```Haskell
ghci> maxBound :: (Bool, Int, Char)
(True,2147483647,'\1114111')
```

### `Num`

`Num` is numeric typeclass. Its members have the property of being able to act like numbers.

```Haskell
ghci> :t (*)  
(*) :: (Num a) => a -> a -> a  
```

因此 `(*)` 可用于任意数字类型。

That's why `(5 :: Int) * (6 :: Integer)` will result in a type error whereas `5 * (6 :: Integer)` will work just fine and produce an `Integer` because 5 can act like an `Integer` or an `Int`.

要称为 `Num`，首先必须是 `Show` 和 `Eq`。

### `Integral`

`Num` 包含 real numbers and integral numbers，而 `Integral` 只包含整数：

* `Int`
* `Integer`

### `Floating`

`Floating` 只包含浮点数：

* `Float`
* `Double`

### `fromIntegral` 函数

`fromIntegral` 类型：

```Haskell
fromIntegral :: (Num b, Integral a) => a -> b
```

它接受一个 `Integral`，返回更通用的 `Num`。

该函数非常实用，因为 Haskell 区分整数和实数，因此 `Int` + `Double` 无法通过类型检查，例如：

```Haskell
length :: [a] -> Int
```

`length` 结果为 `Int`，而非 `Num`，所以：

```Haskell
length [1, 2, 3] + 2.3
```

将报错，借助 `fromIntegral`：

```Haskell
fromIntefral (length [1, 2, 3]) + 2.3
```

运行良好。
