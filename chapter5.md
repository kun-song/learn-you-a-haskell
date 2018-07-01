# 第五章 高阶函数

高阶函数：

* 参数是函数
* 返回值是函数

## 柯里函数（curried functions）

Haskell 中，所有函数 **最多只能有一个** 参数，但前面我们见过很多接受多个参数的函数，这是怎么回事呢？

实际上，所有看似多参的函数都是柯里函数（curried functions），多参只是假象而已。

例如：

```Haskell
max 4 5
```

`max` 看似接受两个参数，但其实 `max` 先应用到 4 上，返回一个函数，然后将该函数应用到 5 上，即：

```Haskell
(max 4) 5
```

`max` 类型：

```Haskell
λ> :t max
max :: Ord a => a -> a -> a
```

函数类型是 **右结合** 的，因此也可以写成 `a -> (a -> a)`。

柯里化函数的好处：

>以部分参数调用柯里化函数，将得到一个 **部分应用函数**（partially applied function），该函数以剩余参数为参数，返回结果与原函数一致。

借助部分应用，可以更便捷的构建新函数，从而方便函数式编程。

例如，将一个 `Int` 与 100 做比较的函数：

```Haskell
compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

λ> compareWithHundred 20
GT
```

因为 `compare` 的类型为 `(Ord a) -> a -> a -> Ordering`，借助部分应用，`compareWithHundred` 可以简化为：

```Haskell
compareWithHundred' :: Int -> Ordering
compareWithHundred' = compare 100
```

`compare 100` 类型为 `(Ord a) -> a -> Ordering`，正好合适。

### 截断

中缀函数也可部分应用，借助截断（section），即将函数调用用 `()` 包裹，然后仅提供一个参数，将得到一个以剩余参数为参数的函数：

```Haskell
dividedByTen :: (Floating a) => a -> a
dividedByTen = (/ 10)

λ> dividedByTen 2
0.2
```

`dividebByTen` 效果与 `(/ 10) 2` 和 `2 / 10` 完全相同。

查看入参是否为大写字母：

```Haskell
isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A' .. 'Z'])

λ> isUpperCase 'a'
False
λ> isUpperCase 'A'
True
```

#### 注意

使用截断唯一需要注意的是 `-`，按照截断定义 `(-10)` 将得到一个函数，但为方便起见，Haskell 认为 `(-10)` 是负 10。

如果想用减法函数，用 `subtract` 替代 `-` 即可：

```Haskell
λ> (subtract 10) 15
5
```

## 更多高阶函数

Haskell 函数可以取函数作为参数，也可返回函数。

```Haskell
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

λ> applyTwice (+ 3) 10
16
λ> applyTwice (/ 3) 15
1.6666666666666667
λ> applyTwice (* 3) 10
90
λ> applyTwice ("Hello" ++) "World"
"HelloHelloWorld"
λ> applyTwice (1 :) [3]
[1,1,3]
```

因为 `->` 右结合，所以 `(a -> a) -> a -> a`，第一个参数为函数 `(a -> a)`。

### 实现 `zipWith`

```Haskell
zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _              = []
zipWith' _ _ []              = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

λ> zipWith' (+) [1, 2, 3] [4, 5, 6]
[5,7,9]
λ> zipWith' (:) [1, 2, 3] [[4], [5], [6]]
[[1,4],[2,5],[3,6]]
λ> zipWith' (++) ["Hello ", "Hi "] ["world", " boy"]
["Hello world","Hi  boy"]
```

### 实现 `flip`

`flip` 接受一个函数参数，返回一个新函数，它与入参的唯一区别是：前两个参数的顺序相反。

```Haskell
flip' :: (a -> b -> c) -> b -> a -> c
flip' f = g
  where g b a = f a b

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f b a = f a b

λ> zip [1, 2, 3] ['a', 'b', 'c']
[(1,'a'),(2,'b'),(3,'c')]
λ> flip' zip [1, 2, 3] ['a', 'b', 'c']
[('a',1),('b',2),('c',3)]

λ> zipWith div [10 ..] [1, 2, 3]
[10,5,4]
λ> zipWith (flip' div) [10 ..] [1, 2, 3]
[0,0,0]
```

## 函数式程序员工具箱

函数式编程：数据转换。

### `map` 函数

实现：

```Haskell
map' :: (a -> b) -> [a] -> [b]
map' _ []       = []
map' f (x : xs) = f x : map' f xs
```

应用：

```Haskell
λ> map' (+ 10) [1, 2, 3, 4]
[11,12,13,14]
λ> map' (++ "!") ["Hello", "Hi"]
["Hello!","Hi!"]
λ> map' (replicate 3) [1, 2, 3]
[[1,1,1],[2,2,2],[3,3,3]]
λ> map' (map' (^ 2)) [[1, 2], [3, 4]]
[[1,4],[9,16]]
λ> map' fst [(1, 'a'), (2, 'b')]
[1,2]λ> map' (+ 10) [1, 2, 3, 4]
[11,12,13,14]
λ> map' (++ "!") ["Hello", "Hi"]
["Hello!","Hi!"]
λ> map' (replicate 3) [1, 2, 3]
[[1,1,1],[2,2,2],[3,3,3]]
λ> map' (map' (^ 2)) [[1, 2], [3, 4]]
[[1,4],[9,16]]
λ> map' fst [(1, 'a'), (2, 'b')]
[1,2]
```

上面应用都可以用 list 推导做，但 `map` 函数更加易读。

### `filter` 函数

实现：

```Haskell
filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []  = []
filter' p (x : xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs
```

应用：

```Haskell
λ> filter' (> 3) [1, 2, 3, 4, 5]
[4,5]
λ> filter' even [1 .. 10]
[2,4,6,8,10]
λ> let notNull x = not (null x) in filter' notNull [[1, 2], [3, 4], [], []]
[[1,2],[3,4]]
λ> filter' (`elem` ['a' .. 'z']) "Hello World!"
"elloorld"
λ> filter' (`elem` ['A' .. 'Z']) "Hello World!"
"HW"
```

类似 `map`，`filter` 也可以用 list 推导替代，看场景，选择可读性更高的做法。

比如，带有多个谓词的 list 推导，需要多次 `filter` 才能实现：

```Haskell
λ> filter (< 15) (filter even [1 .. 20])
[2,4,6,8,10,12,14]
```

等价的 list 推导如下：

```Haskell
λ> [x | x <- [1 .. 20], even x, x < 15]
[2,4,6,8,10,12,14]
```

### `map` 和 `filter` 的更多示例

查找 100000 以下，3829 的最大倍数：

```Haskell
λ> let p x = x `mod` 3829 == 0 in head (filter p [100000, 99999 ..])
99554
```

对小于 10000 的奇数的平方，求和：

```Haskell
λ> sum (takeWhile (< 10000) (filter odd (map (^ 2) [1 ..])))
166650
```

求克拉兹序列：

```Haskell
chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n  = n : chain (n * 3 + 1)
```

以 1-100 所有数字为起始数，有多少克拉兹链的长度大于 15：

```Haskell
λ> length (filter (> 15) (map length (map chain [1 .. 100])))
66
```

### `map` “多参”函数

前面仅使用单参函数调用 `map`，实际上也可以用多参函数：

```Haskell
λ> ((map (*) [1, 2, 3]) !! 2) 10
30
```

`(*)` 是一个“多参”函数，`map (*) [1, 2, 3]` 得到一个函数列表，其中每个函数为 `(1 *)`、`(2 *)` ...

## lambda

因为 `\` 看起来像希腊字母 lambda，因此 Haskell 用 `\` 声明 lambda 表达式：

```Haskell
λ> length (filter (\x -> length x > 15) (map chain [1..100]))
66
```

* `(\x -> length x > 15)` 为 lambda 表达式，习惯用 `()` 包裹；

lambda 表达式中可以用模式匹配，但只能有一个模式分支：

```Haskell
λ> map (\(a, b) -> a + b) [(1, 2), (3, 4)]
[3,7]
```

## `fold` 操作

Haskell 中 list 处理函数基本遵循相同的套路：

* 若是 `[]`，则 ...
* 若是 `(x : xs)`，则 ...

因此 Haskell 提供 `fold` 函数，简化 list 的处理，所有基于 list 遍历并返回一个值的函数都可以用 `fold` 实现。

### `foldl`

不使用显式递归，借助 `foldl` 实现 `sum`：

```Haskell
sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

λ> sum' [1, 2, 3]
6
```

### `foldr`

用 `foldr` 实现 `map`：

```Haskell
map2 :: (a -> b) -> [a] -> [b]
map2 f xs = foldr (\h t -> f h : t) [] xs
```

用 `foldl` 实现 `map`：

```Haskell
map3 :: (a -> b) -> [a] -> [b]
map3 f xs = foldl (\t h -> t ++ [f h]) [] xs
```

但 `++` 效率比 `:` 低很多，因此需要生成列表时，一般用 `foldr`。

实现 `elem` 函数：

```Haskell
elem1 :: (Eq a) => a -> [a] -> Bool
elem1 x xs = foldr (\z acc -> if x == z then True else acc) False xs

λ> elem1 1 [1, 2, 3]
True
λ> elem1 10 [1, 2, 3]
False
```

### `foldl` vs `foldr`

两者最大的区别：`foldl` 无法处理无限列表，`foldr` 可以。

### `foldl1` 与 `foldr1`

`foldl1` 和 `foldr` 与 `foldl` 和 `foldr` 基本相同，只是不需要提供初始值，它们以第一个元素 or 最后一个元素作为初始值。

实现 `maximum`：

```Haskell
maximum1 :: (Ord a) => [a] -> a
maximum1 = foldl1 max
```

>`foldl1` 和 `foldr1` 无法处理空列表，因此只有当确定不会出现空列表时，才可以使用两者。

### `fold` 更多例子

实现 `reverse` 函数：

```Haskell
reverse1 :: [a] -> [a]
reverse1 = foldl (\acc h -> h : acc) []
```

考虑到 `(\acc h -> h : acc)` 作用于 `:` 几乎相同，只有参数顺序相反，因此可以用 `flip` 函数：

```Haskell
reverse2 :: [a] -> [a]
reverse2 = foldl (flip (:)) []

λ> reverse2 [1, 2, 3]
[3,2,1]
```

实现 `product` 函数：

```Haskell
product1 :: (Num a) => [a] -> a
product1 = foldl (*) 1

λ> product1 [1, 2, 3]
6
```

实现 `filter` 函数：

```Haskell
filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p = foldr (\h acc -> if p h then h : acc else acc) []
```

实现 `last` 和 `head` 函数：

```Haskell
last1 :: [a] -> a
last1 = foldl1 (\_ x -> x)

head1 :: [a] -> a
head1 = foldr1 (\x _ -> x)
```

### `fold` 无限列表

下面以 `and` 函数为例，说明为什么 `foldr` 可以处理无限列表，而 `foldl` 不能。

`and` 接受一个 `[Bool]`，若其中有一个 `False`，结果为 `False`，只有全部为 `True` 时，结果为 `True`。

因此 `and` 在遇到一个 `False` 时，就应该直接返回 `False`，不应该再继续计算 list 中剩余元素。

`foldl` 和 `foldr` 分别实现：

```Haskell
and' :: [Bool] -> Bool
and' = foldr (&&) True

and'' :: [Bool] -> Bool
and'' = foldl (&&) True
```

同时，`&&` 类型为：

```Haskell
(&&) :: Bool -> Bool -> Bool
True && x  = x
False && _ = False
```

而 `and'' (repeat False)` 展开为：

// TODO

### 扫描

`scanr` 和 `scanl` 与 `foldr` 和 `foldl` 类似，不过 `scan` 会把 acc 的所有变化记录到一个 list 中：

```Haskell
λ> scanl (+) 0 [1, 2, 3, 4]
[0,1,3,6,10]
λ> scanl1 (+) [1, 2, 3, 4]
[1,3,6,10]

λ> scanr (+) 0 [1, 2, 3, 4]
[10,9,7,4,0]
λ> scanr1 (+) [1, 2, 3, 4]
[10,9,7,4]
```

`scanl` 和 `scanl1` 的最终结果在 **右侧**，而 `scanr` 和 `scanr1` 的最终结果在 **左侧**。

`scan` 用于跟踪基于 `fold` 实现的函数的执行过程。

例如，将自然数的平方根相加，在何处将超过 1000？

```Haskell
λ> (length (takeWhile (< 1000) (scanl1 (+) (map sqrt [1 ..])))) + 1
131
```

## `$` 函数应用操作符

函数应用操作符（function application operator）定义如下：

```Haskell
($) :: (a -> b) -> a -> b
f $ x = f x
```

`$` 也是函数应用，类似空格分开的函数应用，但空格应用优先级最高，而 `$` 优先级最低，且：

* 空格应用 **左结合**
* `$` 应用 **右结合**

借助 `$` 可以减少括号的数量。

例如：

```Haskell
sum (map sqrt [1..20])
```

可改写为：

```Haskell
sum $ map sqrt [1..20]
```

再例如：

```Haskell
sqrt (1 + 2 + 3)
```

可改写为：

```Haskell
sqrt $ 1 + 2 + 3
```

再如：

```Haskell
sum (filter (> 10) (map (* 2) [1..10]))
```

可改写为：

```Haskell
sum $ filter (> 10) (map (* 2) [1..10])
```

因为 `$` 是右结合的，可以继续改写：

```Haskell
sum $ filter (> 10) $ map (* 2) [1..10]
```

* `filter (> 10)` 也是一个函数

除了减少括号的数量，`$` 还可以把一个函数应用转换为函数，这样可以将一个函数应用映射到一个函数列表上：

```Haskell
λ> map ($ 10) [(+ 1), (* 2), (`div` 2)]
[11,20,5]
```

`($ 10)` 是一个函数，它接受一个函数参数，并将该函数应用到 10 上。

## 函数组合

`.` 定义如下：

```Haskell
(.) :: (b -> c) -> (a -> b) -> a -> c
f . g = \x -> f (g x)
```

因此 `f . g` 函数先应用 `g`，然后应用 `f`。

函数组合可以随时生成函数，传递给其他函数，用 lambda 表达式也可以做到同样的事，但函数组合更加简洁。

例如将 `[Int]` 全部转换为负数：

```Haskell
λ> map (\x -> negate (abs x)) [1, -2, 3, -4]
[-1,-2,-3,-4]
```

利用 `.` 更加简洁：

```Haskell
λ> map (negate . abs) [1, -2, 3, -4]
[-1,-2,-3,-4]
```

同时 `.` 是 **右结合**，因此 `f (g (z x))` 等价于 `(f . g . z) x`：

```Haskell
λ> map (\xs -> negate (sum (tail xs))) [[1..5], [3..6], [1..7]]
[-14,-15,-27]

λ> map (negate . sum . tail) [[1..5], [3..6], [1..7]]
[-14,-15,-27]
```

### 多参函数的组合

`f . g` 中的两个函数都是单参函数，对于多参函数，可以先部分应用一些参数，得到一个单参函数，然后进行组合：

```Haskell
sum (replicate 5 (max 4 8))

(sum . replicate 5) (max 4 8)

sum . replicate 5 $ (max 4 8)
```

### Point-Free（pointless） 风格

函数组合的另一个应用是创建 Point-Free 风格的函数。

```Haskell
sum' :: (Num a) -> [a] ->a
sum' xs = foldl (+) 0 xs
```

去掉 `=` 左侧的 `xs` 即为 Point-Free 风格：

```Haskell
sum' :: (Num a) -> [a] -> a
sum' = foldl (+) 0
```

因为 `foldl` 有 3 个参数，借助部分应用，的确可以这样写。

但对于普通函数，直接去掉并不可以：

```Haskell
fn x = ceiling (negate (tan (cos (max 50 x))))
```

不能直接去掉 `fn x` 中的 `x`，所以只能通过将 `=` 改写为函数组合，从而实现 Point-Free 的 `fn` 函数：

```Haskell
n = ceiling . negate . tan . cos . max 50
```

Point-Free 可以使代码更加简洁，但滥用会降低可读性。
