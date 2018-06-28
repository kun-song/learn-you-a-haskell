# 第三章 函数的语法

## 模式匹配

模式匹配两件事：

* 匹配
* 解构

定义函数时，可以为不同模式定义不同函数体：

```Haskell
lucky :: Int -> String
lucky 7 = "Luky, number 7!"
lucky _ = "Sorry, you're out of luck."
```

调用 `lucky` 函数时，将传入的参数 **自上而下** 依次匹配各个模式，即 `7` 和 `_`，若匹配成功，则该模式对应的函数体被调用。

阶乘：

```Haskell
fact :: Int -> Int
fact 0 = 1
fact n = n * fact (pred n)  -- n - 1 也可
```

### 元组

计算二维向量和：

```Haskell
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (a, b) (c, d) = (a + c, b + d)
```

`fst` 和 `snd` 可以取出 pair 中的值，为 3 元组实现类似函数：

```Haskell
first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z
```

### 列表 & 列表推导

列表推导也可用模式匹配：

```Haskell
λ> let xs = [(1, 2), (3, 4), (5, 6)]
λ> [a + b | (a, b) <- xs]
[3,7,11]
```

列表推导时，若模式匹配失败，则跳过该元素，直接处理下一个元素。

列表应用模式匹配：

```Haskell
head' :: [a] -> a
head' []      = error "head on []!"
head' (x : _) = x
```

### as 模式

```Haskell
firstLetter :: String -> String
firstLetter xs @ (x : _) = "The first letter of " ++ xs ++ " is " ++ [x]

λ> firstLetter "Hello"
"The first letter of Hello is H"
```

模式 `xs @ (x : _)` 中，`@` 右侧为匹配的模式，左侧为匹配的值。

## 守卫

单独使用守卫，避免大段 if-else：

```Haskell
bmiTell :: (RealFloat a) => a -> String  
bmiTell bmi  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
```

* `|` 至少缩进一个空格

守卫与模式匹配很契合：如果一个函数的所有守卫都没有通过，则转入下一个 **模式** 继续处理。

用 if-then-else 实现 `max'`：

```Haskell
max' :: (Ord a) => a -> a -> a
max' a b = if (a > b) then a else b
```

用守卫实现：

```Haskell
max' :: (Ord a) => a -> a -> a
max' a b
  | a > b     = a
  | otherwise = b
```

用守卫实现 `compare'` 函数：

```Haskell
compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b  -- 中缀定义
  | a == b    = EQ
  | a <= b    = LT
  | otherwise = GT

λ> compare' 10 2
GT
λ> 10 `compare'` 2
GT
```

>反引号 "`" 可以以中缀形式：
>* 调用函数
>* 定义函数

## where

`where` 用于定义局部变量，避免重复计算：

```Haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= 18.5 = "You're underweight, you emo, you!"  
    | bmi <= 25.0 = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= 30.0 = "You're fat! Lose some weight, fatty!"  
    | otherwise   = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
```

`where` 子句可以有多行，但必须列对齐：

```Haskell
bmiTell :: (RealFloat a) => a -> a -> String  
bmiTell weight height  
    | bmi <= skinny = "You're underweight, you emo, you!"  
    | bmi <= normal = "You're supposedly normal. Pffft, I bet you're ugly!"  
    | bmi <= fat    = "You're fat! Lose some weight, fatty!"  
    | otherwise     = "You're a whale, congratulations!"  
    where bmi = weight / height ^ 2  
          skinny = 18.5  
          normal = 25.0  
          fat = 30.0  
```

### `where` 作用域

`where` 仅对：

* 本函数
* 本函数的特定模式分支

可见。

```Haskell
greet :: String -> String
greet "Juan" = niceGreeting ++ " Juan!"
greet name   = niceGreeting ++ " " ++ name
  where niceGreeting = "Hello "
```

`niceGreeting` 只能用于第二个模式分支，其他分支不可见，因此上面代码无法编译。

### `where` 模式匹配

`where` 绑定可以使用模式匹配：

```Haskell
initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f : _) = firstname
        (l : _) = lastname

λ> initials "Song" "Kun"
"S. K."
```

可以直接在参数上使用模式匹配，此处仅演示 `where` 也能用模式匹配。

### `where` 中的函数

除常量外，`where` 中也可以定义函数：

```Haskell
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi h w | (h, w) <- xs]
  where bmi weight height = weight / height ^ 2
```

* `(h, w) <- xs` 列表推到中的模式匹配

## `let`

`where` 中的常量对整个函数分支可见，所有守卫也可见，而 `let` 常量仅对 `in` 中的表达式可见：

```Haskell
let <bindings> in <expressions>
```

例如：

```Haskell
cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in  sideArea + 2 * topArea
```

`let` 与 `where` 真正的区别是：

* `let` 是表达式，有返回值，几乎可以出现在任何地方，更加灵活

```Haskell
λ> 4 * (let a = 10 in a * 2) + 1
81
```

#### `let` 常见用法

##### 局部作用中，定义函数

```Haskell
λ> [let square x = x * x in (square 1, square 2, square 3)]
[(1,4,9)]
```

##### 绑定多个名字

在一行中绑定多个名字，用分号隔开：

```Haskell
λ> (let a = 100; b = 200; c = 300 in a + b + c, let foo = "Hello"; bar = "world" in foo ++ bar)
(600,"Helloworld")
```

##### 元组结构

`let` + 模式匹配，很方便地从元组取值：

```Haskell
λ> (let (a, b, c) = (1, 2, 3) in a + b + c) * 111
666
```

### 列表推导中的 `let`

用 `let` 重写前面的 `calcBmis`：

```Haskell
calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (h, w) <- xs, let bmi = w / h ^ 2, bmi > 25.0]
```

### GHCi 中的 `let`

GHCi 中，可以省略 `let` 表达式的 `in` 部分，此时 `let` 中的名字对整个会话可见：

```Haskell
λ> let zoot x y z = x + y + z
λ> zoot 1 2 3
6
```

若不省略 `in`，则会话不可见：

```Haskell
λ> let boot x y z = x + y + z in boot 1 2 3
6
λ> boot

<interactive>:1141:1-4: error:
    • Variable not in scope: boot
    • Perhaps you meant one of these:
        ‘Ghci2.zoot’ (imported from Ghci2), ‘zoot’ (line 1139)
λ> 
```

## case 表达式

Java 中的 match 语句只能根据具体值选择执行分支，而 Haskell 的 case 表达式可以根据 **模式匹配** 选择分支。

case 表达式语法：

```Haskell
case expression of pattern -> result  
                   pattern -> result  
                   pattern -> result  
                   ...  
```

>若模式都匹配完了，但没有合适的，抛出运行时异常。

函数定义时对参数进行的模式匹配是 case 表达式的语法糖：

```Haskell
head' :: [a] -> a
head' []      = error "empty on []!"
head' (x : _) = x
```

等价于：

```Haskell
head' :: [a] -> a
head' xs = case xs of
  []      -> error "empty on []!"
  (x : _) -> x
```

模式匹配也是表达式：

```Haskell
describeList :: [a] -> String  
describeList xs = "The list is " ++ case xs of [] -> "empty."  
                                               [x] -> "a singleton list."   
                                               xs -> "a longer list." 
```
