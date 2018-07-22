# 第十二章 Monoid

写 Haskell 代码与写 Java 代码非常不同，在 Haskell 中，我们先用 ADTs 定义类型，然后思考该类型应该有什么样的 **行为**，基于对其行为的期待，让该 type 变成对应 typeclass（定义行为）的实例/成员。

例如，如果我们自定义的 type 可以比较大小，则把它变成 `Eq` typeclass 的实例，如果可以 mapped，则把它变成 `Functor` typeclass 的实例。

`Monoid` typeclass：

```Haskell
class Monoid' m where
  mempty' :: m
  mappend' :: m -> m -> m
  mconcat' :: [m] -> m
  mconcat' = foldr mappend' mempty'
```

* `Monoid` 定义于 `Data.Monoid`；

`Monoid` 由单位元（identity）和一个二元函数（binary function）组成，且两者需要满足 `Monoid` 法则。

注意，单位元是相对二元函数而言的，例如：

* 0 是 `+` 的单位元
* `[]` 是 `++` 的单位元

首先，从 `Monoid` 定义可以看出，`m` 并非 type constructor，而是 concrete type，这与 `Functor`、`Applicative` 不同。

`mempty` 没有参数，因此可以不认为是函数，实际上，它是一个 polymorphic constant，有点类似 `Bounded.minBound`。

`mappend` 将两个 `m` 变成一个，这里 `append` 名字有点误导，实际上，大多数 `Monoid` 实例的 `mappend` 函数都与 append 没有关系 :)

`mconcat` 将一个 `[m]` 归约为 `m`，其默认实现基本够用（个别场景下，默认实现性能不够），因此一般只需要实现 `mempty` 和 `mappend` 两个函数即可。

## `Monoid` 法则

Haskell 允许我们创建 `Monoid` 的实例，同时该实例不遵守 `Monoid` 法则，但这种实例毫无用处，因此涉及 `Monoid` 的函数都会依赖 `Monoid` 法则。

### 单位元法则

```Haskell
mempty `mappend` x == x
x `mappend` mempty == x
```

### 结合律

```Haskell
(x `mappend` y) `mappend` z == x `mappend` (y `mappend` z)
```

## Lists are monoids

list 的 `[]` 和 `++` 可以构成 monoid：

```Haskell
instance Monoid' [a] where
  mempty' = []
  mappend' = (++)
```

* `Monoid` 要求 `m` 是 concrete type，因此是 `[a]` 而非 `[]`；

使用：

```Haskell
λ> [1, 2, 3] `mappend'` [4, 5, 6]
[1,2,3,4,5,6]
λ> "Hello " `mappend'` "World!"
"Hello World!"
λ> "Hello " `mappend'` mempty'
"Hello "
λ> mconcat' ["Hello", " ", "world", "!"]
"Hello world!"
```

## `Product` 和 `Sum`

前面说过，对数字而言，有多种方式可以组成 monoid，例如：

* `+` 和 0
* `*` 和 1

任何一种组合都是合理的，因此 Haskell 没有为我们默认选择一种，而是通过 `newtype` 将它们同时提供。

`Product`：

```Haskell
newtype Product' a = Product' { getProduct' :: a } deriving (Eq, Ord, Read, Show, Bounded)

instance (Num a) => Monoid (Product' a) where
  mempty = Product' 1
  Product' x `mappend` Product' y = Product' $ x * y
```

使用：

```Haskell
λ> Product' 10 `mappend` Product' 5
Product' {getProduct' = 50}
λ> Product' 10 `mappend` mempty
Product' {getProduct' = 10}
λ> mconcat . map Product' $ [1, 2, 3, 4]
Product' {getProduct' = 24}
λ> getProduct' . mconcat . map Product' $ [1, 2, 3, 4]
24
```

`Sum`：

```Haskell
newtype Sum' a = Sum' { getSum' :: a } deriving (Eq, Ord, Read, Show, Bounded)

instance (Num a) => Monoid (Sum' a) where
  mempty = Sum' 0
  Sum' x `mappend` Sum' y = Sum' $ x + y
```

使用：

```Haskell
λ> Sum' 10 `mappend` Sum' 5
Sum' {getSum' = 15}
λ> Sum' 10 `mappend` mempty
Sum' {getSum' = 10}
λ> getSum' . mconcat . map Sum' $ [1, 2, 3, 4]
10
```

## `Any` 和 `All`

类似数字类型，`Bool` 类型也有两种迥然不同的方式成为 monoid：

* `or` + `False`
* `and` + `True`

`Any` 实现了 `or` + `False`：

```Haskell
newtype Any' = Any' { getAny' :: Bool } deriving (Show)

instance Monoid Any' where
  mempty = Any' False
  Any' x `mappend` Any' y = Any' $ x || y
```

使用：

```Haskell
λ> Any' False `mappend` Any' False `mappend` Any' False
Any' {getAny' = False}
λ> Any' False `mappend` Any' False `mappend` Any' True
Any' {getAny' = True}
λ> Any' False `mappend` mempty
Any' {getAny' = False}
λ> Any' True `mappend` mempty
Any' {getAny' = True}
λ> getAny' . mconcat . map Any' $ [False, False, True]
True
```

`All` 实现 `and` + `True`：

```Haskell
newtype All' = All' { getAll' :: Bool } deriving (Show)

instance Monoid All' where
  mempty = All' True
  All' x `mappend` All' y = All' $ (x && y)
```

使用：

```Haskell
λ> getAll' . mconcat . map All' $ [False, False, True]
False
λ> getAll' . mconcat . map All' $ [True, True, True]
True
λ> All' True `mappend` All' True
All' {getAll' = True}
λ> All' True `mappend` All' False
All' {getAll' = False}
```

## `Ordering`

`Ordering` 是 `compare` 函数的结果类型，有 3 种值：

* `LT`
* `EQ`
* `GT`

```Haskell
   10 `compare` 9
=> GT
   10 `compare` 90
=> LT
   10 `compare` 10
=> EQ
```

`Ordering` 也是 monoid，虽然不那么直观：

```Haskell
instance Monoid' Ordering where
  mempty' = EQ
  GT `mappend'` _ = GT
  LT `mappend'` _ = LT
  EQ `mappend'` y = y
```

* `mappend` 背后的逻辑有点类似字符串比较，例如 `ab` 与 `ac`，先比较第一个字符，如果大小有区别，则将其作为整个字符串比较的结果，如果相等，则比较第二个字符...

```Haskell
   LT `mappend` GT
=> LT
   GT `mappend` LT
=> GT
   EQ `mappend` LT
=> LT
   mempty `mappend` LT
=> LT
```

让 `Ordering` 成为 `Monoid` 实例有什么用处呢，看一个例子，比较两个字符串，规则如下：

* 先按长度比较，若长度不同，返回 `LT` 或 `GT`，若长度相同（`EQ`）
* 则按字母序比较

如果没有 `Ordering` monoid，实现为：

```Haskell
compareLength :: String -> String -> Ordering
compareLength x y = let a = length x `compare` length y
                        b = x `compare` y
                    in
                        if a == EQ then b else a

   length "Hello" `compare` length "Hellox"
=> LT
   compareLength "Hello" "HellO"
=> GT
```

但借助 `Ordering` 是 monoid 这点，实现可以简化为：

```Haskell
compareLength' :: String -> String -> Ordering
compareLength' x y = (length x `compare` length y) `mappend'` (x `compare` y)

   compareLength' "Hello" "Hellox"
=> LT
   compareLength' "Hello" "HellO"
=> GT
```

借助 `Monoid`，我们可以把多个判断标准，按照优先级串联起来：

```Haskell
import Data.Monoid  
  
lengthCompare :: String -> String -> Ordering  
lengthCompare x y = (length x `compare` length y) `mappend`  
                    (vowels x `compare` vowels y) `mappend`  
                    (x `compare` y)  
    where vowels = length . filter (`elem` "aeiou")
```

## `Maybe`

`Maybe` 也有多种方式成为 `Monoid` 实例。

### 第一种

第一种方式是，如果 `a` 是 `Monoid`，则 `Maybe a` 也是 `Monoid`，即 `Maybe a` 的 `mappend` 代理给 `a` 的 `mappend` 函数：

```Haskell
instance Monoid' a => Monoid' (Maybe a) where
  mempty' = Nothing
  Nothing `mappend'` m = m
  m `mappend'` Nothing = m
  Just x `mappend'` Just y = Just $ x `mappend'` y
```

* `Maybe a` 成为 `Monoid` 实例的前提：`a` 首先必须是 `Monoid` 实例；

使用：

```Haskell
λ> Just "Hello" `mappend'` Nothing
Just "Hello"
λ> Nothing `mappend'` Just "World"
Just "World"
λ> Just [1, 2, 3] `mappend'` Just [4, 5]
Just [1,2,3,4,5]
```

`Maybe a` 以这种方式成为 `Monoid` 实例，可以让我们组合结果为 `Maybe` 的函数，这样组合时无法考虑该函数返回的是 `Nothing` 还是 `Just`，因为该逻辑已经在 `mappend` 中实现了。

### 第二种

#### `First`

`a` 是 `Monoid` 仅仅是假设而已，`a` 当然可以不是 `Monoid`，回忆一下，其实仅仅当两个参数都是 `Just` 时才用到 `a` 的 `mappend` 函数，所以如果 `a` 不是 `Monoid`，只需要想出一种合理的方式处理两个 `Maybe` 的内容即可。

其中一种合理方式是仅取第一个 `Maybe` 的内容，抛弃第二个 `Monoid` 的内容：

```Haskell
instance Monoid (First a) where
  mempty = First Nothing
  First (Just x) `mappend` _ = First $ Just x
  First Nothing `mappend` f  = f
```

使用：

```Haskell
λ> First (Just "Hello") `mappend` First Nothing
First {getFirst = Just "Hello"}
λ> First Nothing `mappend` First (Just "World")
First {getFirst = Just "World"}
λ> First (Just "Hello") `mappend` First (Just "World")
First {getFirst = Just "Hello"}
λ> First (Just [1, 2, 3]) `mappend` First (Just [4, 5])
First {getFirst = Just [1,2,3]}
```

`First` 使用场景：当有很多 `Maybe`，然后想知道其中是否存在 `Just` 时，可以用 `mconcat`：

```Haskell
λ> getFirst . mconcat . map First $ [Nothing, Nothing, Just 1, Just 2, Just 3]
Just 1
```

#### `Last`

能选第一个，当然也能选第二个：

```Haskell
newtype Last a = Last { getLast :: Maybe a } deriving Show

instance Monoid (Last a) where
  mempty = Last Nothing
  _ `mappend` Last (Just x) = Last (Just x)
  f `mappend` Last Nothing  = f
```

使用：

```Haskell
λ> mconcat . map Last $ [Nothing, Nothing, Just 1, Just 2, Just 3]
Last {getLast = Just 3}
λ> Last (Just "Hello") `mappend` Last (Just "World")
Last {getLast = Just "World"}
```

## `Monoid` + `Folable`

`fold` 函数不仅限于 list，因此可以将能够 fold 的行为定义为 `Foldable` typeclass，`Foldable` 定义于  `Data.Foldable`，因为 `Foldable` 很多函数与 `Prelude` 冲突，所以需要：

```Haskell
import qualified Data.Foldable as F
```

`Foldable` typeclass 定义了 `foldr`、`foldl`、`foldr1`、`foldl1`，不仅 list 可被折叠，`Maybe` 也可以：

```Haskell
λ> F.foldl (+) 6 (Just 10)
16
λ> F.foldr (||) False (Just True)
True
```

前面定义过树：

```Haskell
data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show
```

前面已经将 `Tree` 变成了 `Functor` typeclass 实例，现在将其变成 `Foldable` 实例。

只需要 `foldr` 或 `foldMap` 即可：

```Haskell
λ> :t F.foldMap
F.foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
```

* `(a -> m)` 将普通值转换为 `Monoid`，然后再指定要转换的树 `t a`，`foldMap` 即可借助 `mappend` 将整棵树转换为一个 `Monoid` 值；

```Haskell
instance F.Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r
```

* 实现 `foldMap` 后，即可自动获取 `foldr` 和 `foldl` 等其他 `Foldable` 定义的函数。

有树如下：

```Haskell
λ> let tree = Node 5 (Node 3 (Node 1 Empty Empty) (Node 6 Empty Empty)) (Node 9 (Node 8 Empty Empty) (Node 10 Empty Empty))
```

借助 `Foldbable`，可以很容易折叠它：

```Haskell
λ> F.foldl (*) 1 tree
64800
λ> F.foldl (+) 0 tree
42
```

如果想知道 `tree` 中是否存在 3：

```Haskell
λ> F.foldMap (\x -> Any' $ x == 3) tree
Any' {getAny' = True}
```

* 将 x 转换为 `Any` monoid；

是否有大于 100 的节点：

```Haskell
λ> getAny' $ F.foldMap (\x -> Any' $ x > 100) tree
False
```

将树转换为 list：

```Haskell
λ> F.foldMap (\x -> [x]) tree
[1,3,6,5,8,9,10]
```

* `(\x -> [x])` 将每个结点转换为 singleton list，然后用 `mappend` 将所有 singleton list 组合为一个 list。
