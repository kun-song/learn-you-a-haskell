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






