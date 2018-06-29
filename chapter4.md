# 第四章 你好，递归

## 快速排序

快速排序是一个非常适合递归求解的排序算法：

```Haskell
quickSort :: (Ord a) => [a] -> [a]
quickSort []       = []
quickSort (x : xs) = quickSort [z | z <- xs, z <= x] ++ [x] ++ quickSort [z | z <- xs, z > x]
```

更清晰一点：

```Haskell
quickSort :: (Ord a) => [a] -> [a]
quickSort []       = []
quickSort (x : xs) = 
  let lessOrEqual = quickSort [z | z <- xs, z <= x]
      more = quickSort [z | z <- xs, z > x]
  in lessOrEqual ++ [x] ++ more
  
   quickSort [2, 1, 2, 243, -10]
=> [-10,1,2,2,243]
```

如果不借助 List 推导，则繁琐一点：

```Haskell
quickSort' :: (Ord a) => [a] -> [a]
quickSort' []       = []
quickSort' [x]      = [x]
quickSort' (x : xs) = quickSort' less ++ [x] ++ quickSort' more
  where (less, more) = divide x xs [] []

divide :: (Ord a) => a-> [a] -> [a] -> [a] -> ([a], [a])
divide _ [] less more       = (less, more)
divide x (z : zs) less more
  | z < x = divide x zs (z : less) more
  | otherwise = divide x zs less (z : more)
```


