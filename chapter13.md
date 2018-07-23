# 第十三章 更多 Monad 实例

`Functor` 是能 mapped 的东西，例如将 `(+ 10)` map 到 `[1, 2, 3]`

```Haskell
fmap :: Functor f => (a -> b) -> f a -> f b
```

`Applicative` 是增强的 `Functor`，它的函数可以在 computational context 中，例如将 `[(+ 1), (+ 2), (* 8)]` map 到 `[1, 2, 3]`：

```Haskell
<*> :: Applicative f => f (a -> b) -> f a -> f b
```

`Applicative.pure` 可以将普通值（包括函数）放到 computational context 中，例如 `pure （+ 1）` 可以变成 `Just (+ 1)`，`[(+ 1)]` 等等。

而 `Monad` 是增强的 `Applicative`，它的函数更加特别：

```Haskell
(>>=) :: Monad m => m a -> (a -> m b) -> m b
```

* `>>=` 读作 bind

## `Maybe`

`Maybe` 的 computation context 是值可能存在，也可能不存在，`Just 10` 表示存在，而 `Nothing` 表示不存在。

`Maybe` 作为 `Functor`：

```Haskell
λ> fmap (+ 10) (Just 5)
Just 15
λ> fmap (+ 10) Nothing
Nothing
```

`Maybe` 作为 `Applicative`：

```Haskell
λ> Just (+ 10) <*> Just 5
Just 15
λ> Nothing <*> Just 5
Nothing
λ> Just (+ 10) <*> Nothing
Nothing
```

`Maybe` 作为 `Functor` + `Applicative`：

```Haskell
λ> (+) <$> Just 10 <*> Just 5
Just 15
λ> (+) <$> Just 10 <*> Nothing
Nothing
λ> (+) <$> Nothing <*> Just 5
Nothing
```

为 `Maybe` 实现 `>>=`：

```Haskell
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _  = Nothing
appluMaybe (Just x) f = f x
```

## `Monad` typeclass

`Monad` typeclass：

```Haskell
class Monad' m where
  return' :: a -> m a
  (>>=!) :: m a -> (a -> m b) -> m b
  (>>!) :: m a -> m b -> m b
  x >>! y = x >>=! \_ -> y

  fail' :: String -> m a
  fail' msg = error msg
```

前面说过 `Monad` 是加强版的 `Applicative`，但此时没有添加类似 `class Applicative m => Monad m where` 的类型约束，这是历史问题，按理说需要添加的，但即使没添加，我们也认为所有 `Monad` 首先必须是 `Applicative`。

`return` 函数作用与 `Applicative` 的 `pure` 完全相同，仅仅名字不同而已。

`>>=`/`bind` 函数接受一个 monadic value，以及一个接受 normal value 产生一个 monadic value 的函数，并将该 monadic value 应用与该函数，结果为一个 monadic value。

剩下两个函数比较特殊：

* `>>` 函数几乎不需要关心其实现，因为默认实现在大多数场景下已经适合；
* `fail` 几乎不会在应用代码中使用，而由 Haskell 使用；

将 `Maybe` 实现为 `Monad` 实例：

```Haskell
instance Monad' Maybe where
  return' = Just
  Nothing >>=! _ = Nothing
  Just x >>=! f  = f x
```

使用：

```Haskell
   return' 10 :: Maybe Int
=> Just 10
   Just 10 >>=! \x -> Just $ x + 5
=> Just 15
   Nothing >>=! \x -> return' $ x + 5
=> Nothing
```

### Walk the line

平衡杆：

```Haskell
type Birds = Int
type Pole = (Birds, Birds)
```

飞鸟落在左边 or 右边：

```Haskell
landLeft :: Birds -> Pole -> Pole
landLeft n (l, r) = (l + n, r)

landRight :: Birds -> Pole -> Pole
landRight n (l, r) = (l, r + n)
```

`landLeft` 和 `landRight` 可以连续调用：

```Haskell
   landLeft 1 (landLeft 1 (landRight 3 (0, 0)))
=> (2,3)
```

如果能把参数放到左边，函数放到右边就好了，假设有函数：

```Haskell
x -: f = f x
```

借助 `-:` 函数，可以：

```Haskell
(0, 0) -: landRight 3 -: landLeft 1 -: landLeft 1
```

非常清晰可读！

如何发现何时失去平衡呢，例如：

```Haskell
(0,0) -: landLeft 1 -: landRight 4 -: landLeft (-1) -: landRight (-2)
```

第三步时其实已经失去平衡了，但函数应用的最终结果又是 OK 的，因此需要改写 `landLeft` 和 `landRight`，他们都是可能失败的，因此返回类型改为 `Maybe Pole`：

```Haskell
landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (l, r)
  | abs ((l + n) - r) < 4 = Just (l + n, r)
  | otherwise             = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (l, r)
  | abs (l - (r + n)) < 4 = Just (l, r + n)
  | otherwise             = Nothing
```

使用：

```Haskell
   landLeft 4 (0, 0)
=> Nothing
   landLeft 3 (0, 0)
=> Just (3,0)
```

虽然把返回类型改为 `Maybe` 有大大的好处，但失去了连续应用 `landLeft` 和 `landRight` 的能力：

```Haskell
landLeft 1 (landLeft 1 (landRight 3 (0, 0)))
```

* 因为 `landRight` 和 `landLeft` 返回类型都是 `Maybe Pole`，而它们的参数都是 `Pole`；

哈哈哈，正适合用 `Monad` 的 `>>=` 函数！

```Haskell
   landLeft 3 (0, 0) >>= landRight 2 >>= landLeft 1 >>= landRight 1
=> Just (4,3)
```

因此：

```Haskell
   Nothing >>= landRight 1
=> Nothing
```

所以串联的 `landRight` 和 `landLeft` 中间若出现 `Nothing`，该该错误将一直传播到最终结果，不会丢失：

```Haskell
   return (0,0) >>= landLeft 1 >>= landRight 4 >>= landLeft (-1) >>= landRight (-2)
=> Nothing
```

加入 `return` 使调用更加一致：

```Haskell
   return (0, 0) >>= landLeft 3 >>= landRight 2 >>= landLeft 1 >>= landRight 1
=> Just (4,3)
```

