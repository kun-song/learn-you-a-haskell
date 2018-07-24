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

前面说过 `Monad` 是加强版的 `Applicative`，但 `Monad` typeclass 却没有添加类似 `class Applicative m => Monad m where` 的类型约束，这是历史问题，按理说需要添加的，但即使没添加，我们也认为所有 `Monad` 首先必须是 `Applicative`。

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

## Walk the line

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

可以设计一个函数，无论当前 `Pole` 是否满足平衡条件，都返回 `Nothing`（即不平衡）：

```Haskell
banana :: Pole -> Maybe Pole
banana _ = Nothing
```

加入 `banana` 将导致立即失败：

```Haskell
λ> return (0, 0) >>= landLeft 1 >>= banana >>= landRight 1
Nothing
```

* `banana` 无论接受 `Just x` 还是 `Nothing`，结果都是 `Nothing`；

实际上 `banana` 完全没有必要自定义，可以直接使用 `Monad` 的 `>>` 函数：

```Haskell
(>>) :: Monad m => m a -> m b -> m b
x >> y = x >>= \_ -> y
```

`>>` 虽然忽略了第一个参数，但依然考虑了第一个参数可能成功，也可能失败的场景，即 computation context 被保留：

```Haskell
λ> Nothing >> Just 3
Nothing
λ> Just 3 >> Just 4
Just 4
λ> Just 3 >> Nothing
Nothing
```

* `Nothing >> Just 3` 考虑第一个参数实际计算失败，所以结果为 `Nothing` 是合理的；

用 `>>` 替代 `banana`：

```Haskell
λ> return (0, 1) >>= landLeft 1 >> Nothing >>= landRight 1
Nothing
```

如果不把 `Maybe` 视为 `Monad`，则：

```Haskell
routine :: Maybe Pole  
routine = case landLeft 1 (0,0) of  
    Nothing -> Nothing  
    Just pole1 -> case landRight 4 pole1 of   
        Nothing -> Nothing  
        Just pole2 -> case landLeft 2 pole2 of  
            Nothing -> Nothing  
            Just pole3 -> landLeft 1 pole3  
```

非常繁琐！

`Monad` 的 `>>=` 方法实际封装了对 `Just x` 和 `Nothing` 的判断，以及分别处理。

## `do` 记号

`Monad` 在 Haskell 中非常有用，因此有专门用于 `Monad` 的 `do` 记号。

嵌套的 `>>=`：

```Haskell
λ> Just 11 >>= (\x -> Just "!" >>= (\y -> Just $ show x ++ y))
Just "11!"
```

借助 `do` 可以将嵌套的 `>>=` 改写为：

```Haskell
foo :: Maybe String
foo = do
        x <- Just 11
        y <- Just "!"
        Just $ show x ++ y
```

* `do` 仅仅是 `>>=` 的语法糖；
* `do` 中的任意一个 monadic value 为 `Nothing`，则整个 `do` 表达式结果为 `Nothing`；

走钢丝也可以用 `do` 表达：

```Haskell
routine :: Maybe Pole
routine =
  do
    start <- return (0, 0)
    first <- landLeft 2 start
    second <- landRight 3 first
    landLeft 1 second
```

说实话，这里还不如用 `>>=`：

```Haskell
λ> return (0, 0) >>= landLeft 2 >>= landRight 3 >>= landLeft 1
Just (3,3)
```

