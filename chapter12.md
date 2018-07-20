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

