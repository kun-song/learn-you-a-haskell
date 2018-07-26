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

可以很容易获得 `banana` 和 `>>` 的效果：

```Haskell
routine :: Maybe Pole
routine =
  do
    start <- return (0, 0)
    first <- landLeft 2 start
    Nothing
    second <- landRight 2 first
    landRight 1 second
```

* 在 `do` 表达式中添加一行没有 `<-` 的 monadic value，意思是虽然还是要顺序执行这个 monadic value，但我们忽略它的结果，所以 `Nothing` 等价于 `_ <- Nothing`；
* 虽然忽略了 `Nothing` 的结果，但序列中还是出现了 `Nothing`，所以 `do` 表达式的结果变成 `Nothing`；

>何时用 `do`，何时用 `>>=` 取决于用户，不过上面的例子其实更适合用 `>>=`，因为每次 land 都直接依赖前一次的执行结果，使用 `>>=` 更加清晰；而 `do` 却不需要严格顺序依赖。

### `do` 模式匹配

`do` 中可以使用模式匹配：

```Haskell
justHead :: Maybe String -> Maybe Char
justHead s =
  do
    (x : _) <- s
    return x
```

使用：

```Haskell
   justHead $ Just $ "Hello"
=> Just 'H'
   justHead Nothing
=> Nothing
```

如果模式匹配失败怎么办呢？在函数中失败，在 `let` 中失败，在 `do` 中失败，Haskell 的处理机制不同。

当在 `do` 表达式中匹配失败时，将调用 `Monad` 的 `fail` 函数（注意 `do` 表达式是 `Monad` 的 `>>=` 函数的语法糖），`fail` 函数将根据 `Monad` 实例的不同产生一个 computation context 下的失败，以防止整个程序崩溃退出。

但其默认实现为：

```Haskell
fail :: Monad m => String -> m a
fail msg = error msg
```

从 `fail` 默认实现可看出，当调用 `fail` 时，会调用 `error`，进而导致程序崩溃。

前面不是说 `do` 表达式匹配失败时，会调用 `fail` 以避免崩溃吗？？？

当然，这是有限制的，只有当 `Monad` 的 context 有 fail 这种意义时，对应的 `Monad` 实例将实现自己的 `fail`，从而避免崩溃，例如对于 `Maybe`：

```Haskell
fail :: Monad m => String -> m a
fail _ = Nothing
```

因此 `Maybe` 在 `do` 中匹配失败时，不会崩溃退出，而是产生一个 `Nothing`，这是合理的：

```Haskell
wopwop :: Maybe Char  
wopwop = do  
    (x:xs) <- Just ""  
    return x  
```

>The failed pattern matching has caused a failure within the context of our monad instead of causing a program-wide failure, which is pretty neat.

## 列表 `Monad`

`Maybe` 类型的值，可以视为带有 failure context 的值，通过将其变成 `Monad` 实例，我们把 failure handling 集成到了 `>>=` 函数中。

而通过把 list 变成 `Monad` 实例，可以把 non-determinism 集成到 `>>=` 函数中。

list 可以视为 non-deterministic values：

* 5 是确定性的值，只有一种可能
* `[1, 2, 3]` contains several results, so we can view it as one value that is actually many values at the same time

在 list `Applicative` 中已经领教过 list 的这个特点：

```Haskell
   (+) <$> [1, 2, 3] <*> [4, 5]
=> [5,6,6,7,7,8]
```

list 的 context of non-determinism 可以很容易变成 `Monad`：

```Haskell
instance Monad' [] where
  return' x = [x]
  xs >>=! f = concat $ map f xs
  fail' _   = []
```

* `>>=` 就是 Scala 的 `flatMap`，`concat` 就是 Scala 的 `flatten`；

使用：

```Haskell
λ> return' 1 :: [Int]
[1]
λ> [1, 2, 3] >>=! \x -> [x, -x]
[1,-1,2,-2,3,-3]
```

non-determinism 某种程度上也支持 failure，空列表 `[]` 几乎与 `Nothing` 作用相同，因为 `[]` 也表示没有结果，因此 `fail` 函数被定义为 `[]`，而非 `error msg`：

```Haskell
λ> [] >>= \x -> [1, 2, 3]
[]
λ> [1, 2, 3] >>= \x -> []
[]
```

* 第一个例子，`map f xs` 结果就是空了；
* 第二个例子，`map f xs` 为 `[[], [], []]`，`concat` 之后为 `[]`；

### 嵌套 `>>=` 与 list comprehension

```Haskell
λ> [1, 2, 3] >>= \x -> [4, 5] >>= \y -> return (x + y)
[5,6,6,7,7,8]
```

改写为 `do` 表达式：

```Haskell
sumOfLists :: [Int]
sumOfLists =
  do
    x <- [1, 2, 3]
    y <- [4, 5]
    return $ x + y

λ> sumOfLists
[5,6,6,7,7,8]
```

* `do` 表达式更容易看出 `x` 取 `[1, 2, 3]` 中的所有值，`y` 取 `[4, 5]` 中的所有值，最后 `x + y` 是全组合；

`do` 表达式与 list 推导很像：

```Haskell
λ> [x + y | x <- [1, 2, 3], y <- [4, 5]]
[5,6,6,7,7,8
```

>list 推导和 `do` 表达式一样，都是嵌套 `>>=` 的语法糖！

### 过滤

list 推导可以有过滤条件：

```Haskell
λ> [x | x <- [1 .. 50], '7' `elem` (show x)]
[7,17,27,37,47]
```

过滤在 `Monad` 中怎么体现呢？这就需要 `MonadPlus` typeclass 的 `guard` 函数了。

`MonadPlus` typeclass 是同时也是 `Monoid` 的 `Monad`：

```Haskell
class Monad m => MonadPlus' m where
  mzero' :: m a
  mplus' :: m a -> m a -> m a
```

* `mzero` 与 `mplus` 分别对应 `Monoid` typeclass 的 `mempty` 与 `mappend`；

因为 list 既是 `Monoid` 也是 `Monad`，因此可以成为 `MonadPlus` 的实例/成员：

```Haskell
instance MonadPlus' [] where
  mzero' = []
  mplus' = (++)
```

`guard` 定义如下：

```Haskell
guard :: MonadPlus' m => Bool -> m ()
guard True = return ()
guard False = mzero'
```

* `mzero` 代表 failed computation；
* `()` 读作 unit，类似 `data () = ()`；

`guard` 接受一个 `Bool` 参数，若：

* `True`，则返回一个包含 `()` 的 minimal default context；
* `False`，返回 failed monadic value；

>为什么选择 `()` 值？因为使用 `guard` 时会 **忽略** 其返回值，而 `()` 没有什么含义，正好用来抛弃。

```Haskell
λ> guard (5 > 1) :: [()]
[()]
λ> guard (5 > 10) :: [()]
[]
```

* `[()]`：minimal default context with value `()`
* `[]` failed monadic value

`guard` + `>>` 可实现过滤：

```Haskell
λ> [1, 2, 3, 4] >>= (\x -> (guard $ x `mod` 2 == 0) >> return x)
[2,4]
```

分析：

* 当 `guard` 条件为 `True` 时，结果为无意义的 `return ()`，`>>` 将忽略 `return ()`，并提供有意义的值；
* 当 `guard` 条件为 `False` 时，结果为 `mzero`，对于 list 而言为 `[]`，可以认为是 failed monadic value，`>>` 将立即停止计算；

改写为 `do` 表达式：

```Haskell
evenOnly :: [Int]
evenOnly =
  do
    x <- [1, 2, 3, 4]
    guard $ x `mod` 2 == 0
    return x
```

等价于 list 推导：

```Haskell
λ> [x | x <- [1, 2, 3, 4], x `mod` 2 == 0]
[2,4]
```

## Monad 法则

类似 `Functor` 和 `Applicative`，仅仅通过 `instance` 关键字将某 type 变成 `Monad` typeclass 的实例，并不意味该 type 成为了 monad，这只是语法上的东西，type 必须满足 monad 法则，才能称之为 monad。

type 成为 monad 的条件：

* `instance` 为该 type 实现 `Monad` typeclass 实例；
* 遵守 monad 法则；

Haskell 语法上允许任意 type 成为任意 typeclass 的实例，但 Haskell 无法检测各 typeclass 的法则是否被满足。标准库中的实例肯定是满足的，但当使用自定义类型时，需要特别注意这点。

### Left Identity

若用 `return` 将一个值放到 default minimal context 中，然后借助 `>>=` 将其应用于函数，该过程等价于将该值直接应用于该函数（即不使用 `>>=`）：

```Haskell
return x >>= f 等价于 f x
```

因为 `return` 是将 `x` 放在 default minimal context 中，该 context 非常非常小，该 monadic value 其实只有一个值，即使从直觉上看，也与 `f x` 没有区别。

`Maybe`：

```Haskell
λ> return 10 >>= \x -> Just $ x + 1
Just 11
λ> (\x -> Just $ x + 1) 10
Just 11
```

* `return = Just`

list：

```Haskell
λ> return 10 >>= \x -> [x, -x]
[10,-10]
λ> (\x -> [x, -x]) 10
[10,-10]
```

* `return x = [x]`

### Right Identity

若有一个 monadic value，通过 `>>=` 将其应用于 `return` 函数，结果为原本的 monadic value：

```Haskell
m >>= return 等价于 m
```

例子：

```Haskell
λ> Just "Hello" >>= return
Just "Hello"
λ> [1, 2, 3, 4] >>= return
[1,2,3,4]
λ> putStrLn "Hello" >>= return
Hello
```

对于 list monad，`>>=` 实现为：

```Haskell
xs >>= f = concat $ map f xs
```

当 `f` 为 `return` 时，`[1, 2, 3, 4] >>= return` 首先将 `return` map 到每个元素上，得到 `[[1], [2], [3], [4]]`，然后用 `concat` 拍平之，结果还是 `[1, 2, 3, 4]`。

### Associativity

若有多个用 `>>=` 串联的 monadic function，则各个 monadic function 的嵌套方式不影响结果：

```Haskell
m >>= f >>= g 等价于 m >>= (\x -> f x >>= g)
```

结合律在走钢丝里体现很明显：

```Haskell
return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2
```

应该等价于：

```Haskell
return (0,0) >>= (\x -> landRight 2 x >>= (\y -> landLeft 2 y >>= (\z -> landRight 2 z))) 
```

### 从函数组合的角度看 monad 法则

`(.)` 用于组合函数，它实现如下：

```Haskell
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = \x -> f $ g x
```

如果 `f` 和 `g` 都是 monadic function，即 `b -> m c` 和 `a -> m b`，必须借助 `>>=` 才能组合它们：

```Haskell
(<=<) :: Monad m => (b -> m c) -> (a -> m b) -> (a -> m c)
f <=< g = \x -> g x >>= f
```

使用：

```Haskell
λ> let f x = [x, -x]
λ> let g x = [x + 1, x + 2]

λ> let h = f <=< g
λ> h 2
[3,-3,4,-4]

λ> let k = g <=< f
λ> k 2
[3,4,-1,0]
```

借助 `<=<`，结合律可以表示为：

```Haskell
f <=< g <=< h 等价于 f <=< (g <=< h)
```

left identity 原本为：

```Haskell
return x >>= f === f x
```

`return x >>= f` 可以改写为 `f <=< return`，所以 left identity 可以写作：

```Haskell
f <=< return 等价于 f
```

同样 right identity `m >>= return` 可以写作：

```
return <=< f 等价于 f
```

* `f` 是产生 `m` 的 monadic function；

