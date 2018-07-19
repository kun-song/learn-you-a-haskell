# 第十一章 Applicative 函子

OO 中类型通过集成形成 **树状集成层次**，而 Haskell 则灵活的多，完全没有什么继承树，如果觉得某 type 行为类似某 typeclass，只要将该 type 变成该 typeclass 的实例/成员即可。例如 `Int` type 应该可以比较是否相等，则将其变成 `Eq` typeclass 实例即可。

Haskell 中有很多非常抽象、通用的 typeclass，我们定义完自己的 type 后，应该思考该 type 应该具备哪些行为，然后创建特定 typeclass 实例即可。

## `Functor`

`Functor` typeclass 是指任何能够 mapped 的东西，例如 list、`Maybe`、tree 等：

```Haskell
class Functor' f where
  fmap' :: (a -> b) -> f a -> f b
```

>f 是 computational context，例如 `Maybe` 这个计算环境中，计算可能有值，也可能没有值。

一个 type 想要变成 `Functor` typeclass 的实例，则它的 kind 必须是 `* -> *`，例如 `Maybe` 和 `Either a`。

### `IO` 是 `Functor` 的实例

若一个值的类型是 `IO String`，则表明：

* 该值是一个 I/O action，当被执行时，将与现实世界“交互”，并得到一个字符串；

可以用 `<-` 将 `IO String` 执行后得到的值绑定到一个名字。

`IO` type 是 `Functor` 的实例：

```Haskell
instance Functor' IO where
  fmap' f action = do a <- action
                      return $ f a
```

* 先执行 `action`，然后将 `f` 应用与其结果，最后将应用的结果用 `return` 包裹成一个 `IO a` 值；
* `do` 块的最后一个 action 的结果将作为整个 `do` 的结果；

```Haskell
play1 :: IO ()
play1 = do line <- getLine
           let line' = reverse line
           putStrLn $ "You said " ++ line' ++ " backwards!"
           putStrLn $ "Yes, you really said " ++ line

main :: IO ()
main = play1
```

`getLine` 类型为 `IO Stirng`，因此可以利用 `Functor` 改写为：

```Haskell
play2 :: IO ()
play2 = do line <- fmap' reverse getLine
           putStrLn $ "You said " ++ line ++ " backwards!"
           putStrLn $ "Yes, you really said " ++ line

main :: IO ()
main = play2
```

* `fmap' reverse getLine` 将得到一个新的 `IO String`；

另一个例子：

```Haskell
import Data.Char
import Data.List

play3 :: IO ()
play3 = do line <- fmap' (intersperse '-' . reverse . map toUpper) getLine
           putStrLn line

main :: IO ()
main = play3

λ> main
Hello World!
!-D-L-R-O-W- -O-L-L-E-H
```

### `(->) a` 也是 `Functor` 的实例

函数类型 `a -> b` 可以重写为 `(->) a b`，类似 `Either`，`(->)` 有两个类型参数，其 kind 为 `* -> * -> *`，固定入参以后，`(->) a` 也是一个 `Functor` 实例。

```Haskell
instance Functor' ((->) r) where
  fmap' f g = f . g
```

`fmap'` 类型为 `(a -> b) -> f a -> f b`，具体到 `(->) r`，`fmap'` 类型为：

```Haskell
(a -> b) -> (r -> a) -> (r -> b)
```

恰好是函数组合的功能！所以 `fmap'` 可以直接替换为 `.`：

```Haskell
instance Functor' ((->) r) where
  fmap' = (.)
```

`fmap'` 和 `.` 作用完全相同：

```Haskell
λ> (+ 10) . (* 3) $ 4
22
λ> (+ 10) `fmap'` (* 3) $ 4
22
```

### `fmap` 类型深入思考

`fmap'`：

```Haskell
fmap :: (a -> b) -> f a -> f b
```

因为 `->` 左结合，所以：

```Haskell
fmap :: (a -> b) -> (f a -> f b)
```

因此可以认为 `fmap` 接受一个 `a -> b` 函数，返回一个 `f a -> f b` 函数。

>这被称为 lift a function。

```Haskell
λ> :t fmap (+ 10)
fmap (+ 10) :: (Num b, Functor f) => f b -> f b
λ> :t fmap (replicate 2)
fmap (replicate 2) :: Functor f => f a -> f [a]
```

`fmap (+ 10)` 类型为 `(Num b, Functor f) => f a -> f b`，因此可以用于任何可以包裹 `Num` 的 `Functor` 上，例如：

```Haskell
λ> fmap (+ 10) [1, 2, 3]
[11,12,13]
λ> fmap (+ 10) (Just 9)
Just 19
```

### `Functor` 法则

一个 type 要变成 `Functor` typeclass 的实例，需要满足 `Functor` 法则。

我们可以自由地用 `instance` 创建 `Functor` 实例，但 Haskell 无法自动保证我们创建的这些实例满足 `Functor` 法则，所以只有用户自行测试保证。

#### 1. id 法则

`fmap id = id`

分析：`fmap id f a => id f a = f a`

If we **map** the `id` function over a functor, the functor that we get back should be the same as the original functor.

将 `id` map 到 `Functor` 上的效果，与将 `id` 应用到 `Functor` 上的效果相同。

从一些 `Functor` 实例的实现上，可以明显看出 id 法则是否成立：

```Haskell
instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap f Nothing  = Nothing
```

* `f` 若是 `id` 函数，则 `fmap` 明显啥都没做，因此对于 `Maybe` 而言，`fmap id = id` 成立！

#### 2. 结合律

`fmap (f . g) = fmap f . fmap g`

or

`fmap (f . g) F = fmap f (fmap g F)`

Composing two functions and then mapping the resulting function over a functor should be the same as first mapping one function over the functor and then mapping the other one.

对于 `Maybe`：

* `fmap (f . g) Nothing == Nothing`，且 `fmap f (fmap g Nothing) == fmap f Nothing == Nothing`，成立
* `fmap (f . g) (Just x) = Just ((f . g) x)`，且 `fmap f (fmap g (Just x)) == fmap f (Just (g x)) == Just (f (g x)) == Just ((f . g) x)`，也成立

因此 `Maybe` 满足 `Functor` 结合律。

#### 不满足 `Functor` 法则的 `Functor` 实例不是 `Functor`

可以为任意 kind 为 `* -> *` 的 type constructor 创建 `Functor` 实例（借助 `instance` 关键字），但这还不够，必须还要满足 `Functor` 法则，才能认为该 type 是一个 `Functor`。

例如：

```Haskell
data CMaybe a = CNothing | CJust Int a deriving (Show)
```

为 `CMaybe` 创建 `Functor` 实例：

```Haskell
instance Functor CMaybe where
  fmap f CNothing    = CNothing
  fmap f (CJust c x) = CJust (c + 1) (f x)
```

但实际上，`CMaybe` 既无法满足 `id` 法则，也无法满足结合律，因此虽然 `CMaybe` 是 `Functor` typeclass 的成员（`instance`），但它并不是一个 functor。

#### `fmap` 的作用（重要）

可以将 `Functor` 理解为 **产生** 值的东西，而 `fmap f fa` 则对 `fa` 产生的值施加额外的数据转换（通过 `f` 完成），例如：

* `fmap (+3) [1, 2, 3]`，转换函数为 `(+3)`
* `fmap (+3) (*2)` 转换函数为 `(+3)`，对 `(*2)` 产生的值进行转换

## Applicative functors

applicative functors 在 Haskell 中用 `Applicative` typeclass 表示，定义在 `Control.Applicative` 模块中。

前面 `fmap` 的参数 kind 都是 `* -> *`，如果被映射的函数 kind 为 `* -> * -> *` 会发生什么呢？

例如 `fmap (*) (Just 10)`，根据 instance 的实现，结果是 `Just (* 10)`，`Maybe` 中包裹的变成函数 `(* 10)`。

现在若有 `Just (* 10)` 和 `Just 11` 两个 `Functor`，如何将 `(* 10)` 应用于 `11` 呢？？？

此时需要 `Applicative` typeclass，其定义如下：

```Haskell
class (Functor f) => Applicative' f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
```

* 一个 type 想要成为 `Applicative`，首先必须是 `Functor`；
* `<*>` 类型与 `fmap` 有点类似：`fmap :: (a -> b) -> f a -> f b`；

为 `Maybe` 实现 `Applicative` 实例：

```Haskell
instance Applicative' Maybe where
  pure = Just
  (Just f) <*> (Just x) = Just $ f x
  _ <*> _ = Nothing
```

因为 `Functor f => Applicative f`，所以 `<*>` 可以借助 `fmap` 实现：

```Haskell
instance Applicative' Maybe where
  pure = Just
  (Just f) <*> fa = fmap f fa
  Nothing <*> _   = Nothing
```

使用：

```Haskell
λ> Just (* 6) <*!> Just 111
Just 666
λ> pure' (* 6) <*!> Just 111
Just 666
λ> pure' (*) <*!> Just 100 <*!> Just 50
Just 5000
```

* `pure (* 6)` 与 `Just (* 6)` 完全相同；
* `<*!>` 是左结合的；

`pure f <$> Just 10` 实际与 `fmap f (Just 10)` 等价，实际上，`<$>` 仅仅是 `fmap` 的中缀形式：

```Haskell
(<$>) :: (Funtor f) => (a -> b) -> f a -> f b
f <$> x = fmap f x
```

* 注意：类型中的变量与参数、函数中的变量完全隔离，因此两个 `f` 完全独立，不是一回事；

不要混淆 `<$>`（`fmap`） 和 `<*>`：

```Haskell
(++) <$> Just "Hello" <*> "World"
```

### 其他 `Applicative` 实例

#### 1. list

list，更确切的是 `[]` type constructor，也是 applicative：

```Haskell
instance Applicative' [] where
  pure' x = [x]
  fs <*!> xs = [f x | x <- xs, f <- fs]
```

`<*>` 将 `fs` 中的每个函数应用于 `xs` 中的每个元素上：

```Haskell
λ> [(+ 1), (+ 2)] <*!> [1, 2, 3, 4]
[2,3,3,4,4,5,5,6]

λ> [(*), (+)] <*!> [1, 2, 3] <*!> [4, 5, 6]
[4,5,8,6,12,7,5,6,10,7,15,8,6,7,12,8,18,9]
```

#### 2. IO

`IO` 也是 `Applicative` 实例：

```Haskell
instance Applicative' IO where
  pure' = return
  x <*!> y = do
               f <- x
               a <- y
               return $ f a
```

`<*!>` 在 `IO` 实例这，有 sequence 两个 I/O action 的意思。

例如：

```Haskell
myAction1 :: IO String
myAction1 =
  do
    a <- getLine
    b <- getLine
    return $ a ++ b
```

两个 `getLine` 是先后发生的，可以用 `Applicative` 改写：

```Haskell
myAction2 :: IO String
myAction2 = (++) <$> getLine <*> getLine
```

使用：

```Haskell
main :: IO ()
main =
  do x <- myAction2
     putStrLn x

λ> main
Hello
World
HelloWorld
```

#### 3. `(->) r`

前面说过 `r ->` 是 `Functor`，其实它们也是 `Applicative`。

```Haskell
instance Applicative' ((->) r) where
  pure' x = (\_ -> x)
  f <*!> g = \x -> f x $ g x
```

#### 4. `ZipList`

>`ZipList` 位于 `Control.Applicative`。

整数 `*` 和 `+` 都可以成为 `Monoid`，同样 list 也有多种成为 `Applicative` 的方式。

前面 list 已经以全组合方式成为了 `Applicative`：

```Haskell
[(* 3), (+ 1)] <*> [1, 2]
>[3, 6, 2, 3]
```

list 也可以以 zip 的方式成为 `Applicative`：

```Haskell
[(* 3), (+ 1)] <*> [1, 2]
[3, 3]
```

同时，因为同一个 type + 同一个 typeclass 不能有两个实例，因此用 `ZipList a` 表示：

```Haskell
instance Applicative' ZipList where
  pure' x = ZipList (repeat x)
  ZipList fs <*!> ZipList xs = ZipList $ zipWith (\f x -> f x) fs xs
```

* `ZipList` 只有一个字段，类型为 list；

因为 `ZipList a` 没有 `Show` 实例，所以用 `getZipList` 获取：

```Haskell
λ> getZipList $ (+) <$> ZipList [1, 2, 3] <*> ZipList [50, 100, 150]
[51,102,153]
```

### `liftA2`

`Control.Applicative` 定义了 `liftA2` 函数：

```Haskell
liftA2' :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2' f fa fb = f <$> fa <*> fb
```

从两个方面理解：

1. `liftA2` 表明 `Applicative` 比 `Functor` 更加强大，因为借助 `Applicative` 可以多次应用一个函数；
2. `liftA2` 可以理解为将 `a -> b -> c` 函数转换为 `f a -> f b -> f c`；

我们可以把两个 `Applicative` 合成一个 `Applicative`，该 `Applicative` 中包裹前面两个的值。

例如，将 `Just 3` 和 `Just 4` 合成一个：

```Haskell
   (\x -> [x]) <$> Just 4
=> Just [4]

   (:) <$> Just 3 <*> Just [4]
=> Just [3,4]

   liftA2' (:) (Just 3) (Just [4])
=> Just [3,4]

   liftA2' (:) (Just 2) (Just [3, 4])
=> Just [2,3,4]
```

### `sequenceA`

实际上，可以把 **任意数量** 的 `Applicative` 合成一个 `Applicative`：

```Haskell
sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' []       = pure []
sequenceA' (f : fs) = liftA2' (:) f (sequenceA' fs)

   sequenceA [Just 1, Just 2, Just 3]
=> Just [1,2,3]
```

另外，几乎所有涉及 list 遍历的函数都可以用 `fold` 实现：

```Haskell
sequenceA'' :: (Applicative f) => [f a] -> f [a]
sequenceA'' = foldr (liftA2' (:)) (pure [])
```

一些 `sequenceA` 的例子：

```Haskell
   sequenceA'' [Just 1, Just 2, Just 3]
=> Just [1,2,3]
   sequenceA'' [Just 1, Just 2, Nothing]
=> Nothing
   sequenceA'' [[1, 2, 3], [4, 5]]
=> [[1,4],[1,5],[2,4],[2,5],[3,4],[3,5]]
```

当有一个输入参数，和一系列函数，想要判断该参数是否全部满足这些函数时，`sequenceA` 很有用：

```Haskell
   sequenceA' [(> 10), (< 20), odd] 13
=> [True,True,True]
   and $ sequenceA' [(> 10), (< 20), odd] 13
=> True
```

* `sequenceA` 将 `(Num a) => [a -> Bool]` 转换为 `(Num a) => a -> [Bool]`；

list 也是 `Applicative`，当 `sequenceA` 用于 list 时比较有趣：

```Haskell
   sequenceA [[1, 2, 3], [4, 5], [111]]
=> [[1,4,111],[1,5,111],[2,4,111],[2,5,111],[3,4,111],[3,5,111]]
```

结果是所有元素的全组合，回想下 `sequenceA` 的实现：

```Haskell
sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' []       = pure []
sequenceA' (f : fs) = (:) <$> f <*> (sequenceA' fs)
```

推导过程如下：

* We start off with sequenceA `[[1,2],[3,4]]`
* That evaluates to `(:) <$> [1,2] <*> sequenceA [[3,4]]`
* Evaluating the inner sequenceA further, we get `(:) <$> [1,2] <*> ((:) <$> [3,4] <*> sequenceA [])`
* We've reached the edge condition, so this is now `(:) <$> [1,2] <*> ((:) <$> [3,4] <*> [[]])`
* Now, we evaluate the `(:) <$> [3,4] <*> [[]]` part, which will use `:` with every possible value in the left list (possible values are 3 and 4) with every possible value on the right list (only possible value is `[]`), which results in `[3:[], 4:[]]`, which is `[[3],[4]]`. So now we have `(:) <$> [1,2] <*> [[3],[4]]`
* Now, `:` is used with every possible value from the left list (1 and 2) with every possible value in the right list ([3] and [4]), which results in `[1:[3], 1:[4], 2:[3], 2:[4]]`, which is `[[1,3],[1,4],[2,3],[2,4]`

>关键是理解 `<$>` 和 `<*>` 在 list applicative 中的实现细节。

`sequenceA` 用于 `IO` 时：

```Hasekll
ghci> sequenceA [getLine, getLine, getLine]  
heyh  
ho  
woo  
["heyh","ho","woo"]  
```

## `newtype` 关键字

目前关于类型的关键字有：

* `data`：创建 ADTs
* `type`：创建类型别名

`newtype` 用于从已存在的 type 中创建新的 type。

前面说过，list 可以以多种方式成为 `Applicative`，例如全组合：

```Haskell
   [(+ 1), (* 2)] <*> [1, 2, 3]
=> [2,3,4,2,4,6]
```

还可以用 zip 风格，但 list 已经成为了 `Applicative` 实例，且 Haskell 只允许存在一个实例，所以我们用 `ZipList` 实现 zip 风格的 list `Applicative`：

```Haskell
ghci> getZipList $ ZipList [(+ 1),(* 100),(* 5)] <*> ZipList [1, 2, 3]
[2,200,15]
```

* `ZipList` 只有一个 list 字段，可以认为是 list 的包装类，`ZipList` 本身被实现为 `Applicative` 实例，因此当需要用 zip 方式使用 list applicative 时，将 list 包装在 `ZipList` 中即可，操作完成后用 `getZipList` 获取里面包装的 list 值；

那 `ZipList` 是怎么实现的呢，首先当然可以用 `data` 声明：

```Haskell
data ZipList a = ZipList [a]
```

也可以用 record 语法声明，从而获取 `getZipList` 函数：

```Haskell
data ZipList' a = ZipList' { getZipList' :: [a] }
```

The `newtype` keyword in Haskell is made exactly for these cases when we want to just take one type and wrap it in something to present it as another type，`ZipList` 实际上是用 `newtype` 实现的：

```Haskell
newtype ZipList' a = ZipList' { getZipList' :: [a] }
```

与 record 语法相比，仅仅将 `data` 替换成 `newtype`。

`newtype` 要比 `data` 运行效率要高一些，因为使用 `data` 有 wrapping/unwrapping 的开销，而 `newtype` 则让编译器知道，新的类型与老的类型仅仅是类型不同，底层表示是一模一样的，因此省去了 wrapping/unwrapping 的开销。

`newtype` 不够灵活：

* 只能有一个 value constructor
* 该 value constructor 只能有恰好一个字段

`newtype` 中也可以使用 `deriving` 自动生成内置 typeclass（`Eq`, `Ord`, `Enum`, `Bounded`, `Show` and `Read`）的实例，不过前提是被包裹的 type 早已经实现对应的 typeclass 实例：

```Haskell
newtype CharList = CharList { getCharList :: [Char] } deriving (Show, Eq)

   CharList ['a', 'b']
=> CharList {getCharList = "ab"}
   CharList ['a', 'b'] == CharList ['b', 'c']
=> False
   CharList "Hello"
=> CharList {getCharList = "Hello"}
```

其中 value constructor `CharList` 也是普通函数，类型为：

```Haskell
CharList :: [Char] -> CharList
```

而 `getCharList` 类型为：

```Haskell
getCharList :: CharList -> [Char]
```






