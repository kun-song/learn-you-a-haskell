# 第四章 你好，递归

递归是一种思考方式，用于描述问题和问题的解，描述好后，递归解法就出来了。

而命令式则注重于求解步骤，更加细节。

## 不可思议的最大值

查找 `[a]` 中的最大值，其中 `a` 属于 `Ord` type class。

命令式语言中，需要一个保存最大值的变量，然后循环遍历整个 `[a]`，若当前元素大于最大值，则将最大值设为该元素 ...，遍历结束，变量中的值即为最大值。

递归方式：

* 截止条件：若为 `[]`，则不存在最大值
* 若为氮元素列表，则该元素即为列表的最大值
* 若为非空列表 `(x : xs)`，则最大值为 `x` 和 `xs` 列表最大值之间的较大者

```Haskell
maximum' :: Ord a => [a] -> a
maximum' []       = error "maximum' on []"
maximum' [x]      = x
maximum' (x : xs) = x `max` maximum' xs
```

## 更多递归函数

下面实现几个 Haskell 内置的递归函数。

### `replicate`

```Haskell
replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

λ> replicate' 2 33
[33,33]
```

### `take`

```Haskell
take' :: Int -> [a] -> [a]
take' _ []         = []
take' n _ | n <= 0 = []
take' n (x : xs)   = x : take' (n - 1) xs

λ> take' 3 [1, 2, 3, 4]
[1,2,3]
```

`take' n | n <= 0 ...` 没有使用 `otherwise`，因此该分支若不匹配，则跳到下一个分支。

### `reverse`

```Haskell
reverse' :: [a] -> [a]
reverse' []       = []
reverse' (x : xs) = reverse' xs ++ [x]

λ> reverse' [1, 2, 3]
[3,2,1]
```

### `repeat`

```Haskell
repeat' :: a -> [a]
repeat' x = x : repeat' x
```

`repeat'` 没有截止条件，因此会无限增长，可以用 `take` 截取一部分：

```Haskell
take' 10 (repeat' 3)

λ> take' 10 (repeat' 3)
[3,3,3,3,3,3,3,3,3,3]
```

### `zip`

```Haskell
zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

λ> zip' [] [1, 2, 3]
[]
λ> zip' [1, 2, 3] ["a", "b", "c", "d"]
[(1,"a"),(2,"b"),(3,"c")]
```

### `elem`

```Haskell
elem' :: Eq a => a -> [a] -> Bool
elem' _ []      = False
elem' x (h : t) = x == h || x `elem'` t

λ> 1 `elem'` [1, 2, 3]
True
λ> 1 `elem'` [2, 3]
False
```

## 排序


