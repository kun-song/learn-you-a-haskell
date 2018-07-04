# 第七章 构造自己的 type 和 type class

## Algebraic data types 介绍

使用 `data` 关键字定义 type：

```Haskell
data Bool = True | False
```

* `=` 左侧为 type 名字
* `=` 右侧为 value constructors
* type 名字和 value constructor 首字母都必须大写

value constructors 确定该 type 可以取的不同值。

类似 `Int` 可以定义为：

```Haskell
data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647  
```

形状可以定义为：

```Haskell
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
```

* `Circle` value constructor 具有 3 个字段，类型都是 `Float`
* `Rectangle` value constructor 具有 4 个字段，类型都是 `Float`

定义 type 时，value constructor 后面可以跟类型名字，表示该 value constructor 构建的值将包含这些字段的值。

Haskell 没有对象的概念，因此 Haskell 中的字段一般指 value constructor 后面的参数。

value constructor 实际为函数，返回该 type 类型的值：

```Haskell
λ> :t Circle
Circle :: Float -> Float -> Float -> Shape
λ> :t Rectangle
Rectangle :: Float -> Float -> Float -> Float -> Shape
```

计算 `Shape` 的表面积：

```Haskell
surface :: Shape -> Float
surface (Circle _ _ r)          = pi * r ^ 2
surface (Rectangle x1 y1 x2 y2) = (abs $ x1 - x2) * (abs $ y1 - y2)

λ> surface $ Circle 0 0 10
62.831856
```

* `surface` 类型为 `Shape -> Float`，但我们不能写 `Circle -> Float`，因为 `Circle` 仅仅是 value constructor 而已，并不是 type，就像不能写 `True -> Float` 一样
* 可以对 value constructors 使用模式匹配

可 `Circle 0 0 10` 无法输出在终端中：

```Haskell
λ> show $ Circle 0 0 19

<interactive>:2286:1-20: error:
    • No instance for (Show Shape) arising from a use of ‘show’
    • In the expression: show $ Circle 0 0 19
      In an equation for ‘it’: it = show $ Circle 0 0 19
```

因为 Haskell 根本不知道如何输出 `Cirlce 0 0 10`，让 `Shape` type 变成 `Show` type class 的成员就可以了：

```Haskell
data Shape = Circle Float Float Float | Rectangle Float Float Float Float
  deriving (Show)

λ> show $ Circle 0 0 19
"Circle 0.0 0.0 19.0"
```

* `deriving (Show)` 将使 Haskell 自动把 `Shape` 变成 `Show` typeclass 的成员类型

既然 value constructors 不过是普通函数，那就能用于函数组合、部分应用等：

```Haskell
λ> map (Circle 0 0) [1, 2, 3]
[Circle 0.0 0.0 1.0,Circle 0.0 0.0 2.0,Circle 0.0 0.0 3.0]
```

定义二维点：

```Haskell
data Point = Point Float Float deriving (Show)
```

* 此处 data type 名字与 value constructors 名字相同，没啥特别，只有一个 value constructors 时常常这么做
* 左侧的 `Point` 为类型名，右侧的 `Point` 为 value constructors，Haskell 明确区分两者，人阅读时觉得两者一致而已

重新定义 `Shape`：

```Haskell
data Shape = Circle Point Float | Rectangle Point Point
  deriving (Show)
```

修改 `surface` 函数：

```Haskell
surface :: Shape -> Float
surface (Circle _  r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)

λ> surface $ Circle (Point 0 0) 10
62.831856
```

移动 `Shape`：

```Haskell
nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))
```

## `Record` 语法

可以把 `Person` 定义为 type：

```Haskell
data Person = Person String String Int Float String String deriving (Show)
```

可以定义一些获取 `Person` 字段的函数：

```Haskell
firstName :: Person -> String  
firstName (Person firstname _ _ _ _ _) = firstname  
  
lastName :: Person -> String  
lastName (Person _ lastname _ _ _ _) = lastname  
  
age :: Person -> Int  
age (Person _ _ age _ _ _) = age  
  
height :: Person -> Float  
height (Person _ _ _ height _ _) = height  
  
phoneNumber :: Person -> String  
phoneNumber (Person _ _ _ _ number _) = number  
  
flavor :: Person -> String  
flavor (Person _ _ _ _ _ flavor) = flavor
```

以上可读性很差，可以用 record 语法改写：

```Haskell
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String
                     } deriving (Show)
```

record 语法可以让 Haskell 自动生成 `firstName`、`lastName` 等函数，用于获取字段值：

```Haskell
λ> :t lastName
lastName :: Person -> String
```

record 语法还可以让 `deviring (Show)` 生成更加可读的输出：

```Haskell
λ> Person "Song" "Kun" 20 170.0 "1234567890" "tennis"
Person {firstName = "Song", lastName = "Kun", age = 20, height = 170.0, phoneNumber = "1234567890", flavor = "tennis"}
```

而且 record 语法让创建 `Person` 时，必须按照定义时顺序指定各个字段：

```Haskell
λ> Person { flavor = "xx", phoneNumber = "111111", height = 170, age = 20, firstName = "Song", lastName = "Kun" }
Person {firstName = "Song", lastName = "Kun", age = 20, height = 170.0, phoneNumber = "111111", flavor = "xx"}
```

### record 总结

* 自动生成 getter 函数
* `show` 更加可读
* 创建实例时，可以乱序指定字段值

## 类型参数

value constructor 可以有 type parameter：

```Haskell
data Maybe a = Nothing | Just a
```

`a` 即为 type parameter，由于类型参数的存在，我们称 `Maybe` 为 type constructor。

通过提供实际类型，`Maybe` 会变成 `Maybe Int`、`Maybe Char` 等。

```Haskell
λ> Just "Hello"
Just "Hello"
λ> Just 99
Just 99
λ> :t Just "Hello"
Just "Hello" :: Maybe [Char]
λ> :t Just 99
Just 99 :: Num a => Maybe a
λ> :t Nothing
Nothing :: Maybe a
```

`Nothing` 类型为 `Maybe a`，因此可以用于任何需要 `Maybe a` 的地方，比如 `Maybe Int`。

### `data` 中的类型约束

`data` 也可以有类型约束：

```Haskell
data (Ord k) => Map k v = ... 
```

但 Haskell 强烈建议 **never add typeclass constraints in data declarations**，原因参见 [learn you a haskell](http://learnyouahaskell.com/making-our-own-types-and-typeclasses)。

简单说，不管 `data` 中是否加 `(Ord k)` 约束，任何使用 `Map` 的函数，如果需要 `key` 是有序的，则该函数自己需要加 `(Ord k)` 约束，而如果函数根本不关心 `key` 是否有序，`data` 中额外的类型约束会强制函数假设 `key` 有序，不灵活。

因此约束最好放在函数定义时加，不要在 `data` 中加。

定义 3 维向量：

```Haskell
data Vector a = Vector a a a deriving (Show)
```

* 此处没有加 `(Num a)` 类型约束！

一些操作 `Vector` 的函数：

```Haskell
vPlus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector a b c) `vPlus` (Vector d e f) = Vector (a + d) (b + e) (c + f)

vectMulti :: (Num a) => Vector a -> a -> Vector a
(Vector a b c) `vectMulti` m = Vector (a * m) (b * m) (c * m)

scalarMulti :: (Num t) => Vector t -> Vector t -> t
(Vector a b c) `scalarMulti` (Vector d e f) = a * d + b * e + c * f
```

`Vector a` 和 `Vector a a a` 都是合法的写法，但两者完全不同：

* `Vector a` 是 type constructor
* `Vector a a a` 是 value constructor

## 派生实例（Derived instances）

typeclass 是一组函数的集合，该函数集合定义了一些行为。

>A type can be made an **instance** of a typeclass if it supports that behavior. 

```Haskell
type -- instance of a typeclass
```

至于该 type 如何支持 typeclass 的行为，不同语言有不同的实现机制。

例如:

* `Int` type 是 `Eq` typeclass 的实例，因为 `Eq` 定义的行为是判等，而整数自然可以判等；

因为 `Int` 是 `Eq` typeclass 的实例，因此 `Int` 可以使用 `Eq` 定义的函数 `==` 和 `/=`。

Haskell 可以自动将我们的 type 变成以下 typeclass 的实例：

* `Eq`, `Ord`, `Enum`, `Bounded`, `Show`, `Read`

考虑：

```Haskell
data Student = Student { firstName :: String
                       , lastName :: String
                       , age :: Int}
```

假设这 3 个字段可以唯一确定一个学生，则最好能够用 `==` 比较两个 `Student` 值是否相等，将 `Student` 变成 `Eq` typeclass 的实例即可：

```Haskell
data Student = Student { firstName :: String
                       , lastName :: String
                       , age :: Int}
               deriving (Eq)
```

使用 `==` 或 `/=` 比较两个 `Student` 类型的值时，编译器会：

1. 两个值的 value constructor 是否匹配，若匹配
2. 使用 `==` or `/=` 比较各个字段是否匹配（因此，字段类型也必须是 `Eq` typeclass 的实例）

```Haskell
λ> let kun = Student { firstName = "Kun", lastName = "Song", age = 20 }
λ> let kyle = Student { firstName = "Kyle", lastName = "Song", age = 20 }
λ> kun == kyle
False
λ> kun == kun
True
```

现在 `Student` 已经是 `Eq` typeclass 的成员了，所以它可以用于所有类型约束为 `Eq a` 的函数，例如 `elem`：

```Haskell
λ> kun `elem` [kun, kyle]
True
```

`Show` typeclass 是可以转换为 String 的类型集合，`Read` typeclass 是可以从 String 转换而来的类型集合：

```Haskell
data Student = Student { firstName :: String
                       , lastName :: String
                       , age :: Int
                       }
               deriving (Eq, Show, Read)
```

现在可以使用 `show` 将 `Student` 类型的值转换为字符串：

```Haskell
λ> show Student { firstName = "Song", lastName = "Kun", age = 20 }
"Student {firstName = \"Song\", lastName = \"Kun\", age = 20}"
```

`Read` 在某种程度上与 `Show` 相反，它接受一个 String，然后生成对应 type 的值：

```Haskell
λ> read "Student {firstName = \"Song\", lastName = \"Kun\", age = 20}" :: Student
Student {firstName = "Song", lastName = "Kun", age = 20}
```

* 使用 `read` 必须显式指定要转换成的 type

若 Haskell 能推断出 `read` 的目标类型，则不需要显式指定类型：

```Haskell
λ> let kun = Student { firstName = "Kun", lastName = "Song", age = 20 }
λ> read "Student {firstName = \"Song\", lastName = \"Kun\", age = 20}" == kun
False
```

`read` 也可以处理参数类型，不过需要指定具体类型，因此：

```Haskell
read "Just 'x'" :: Maybe a
```

将报错，指定具体类型后没问题：

```Haskell
λ> read "Just 'x'" :: Maybe Char
Just 'x'
```

也可以自动派生 `Ord` typeclass 的实例，规则为：

* 若两个值的 value constructor 不同，则 `data` 中先定义的 value constructor 构造的值小于后定义的 value constructor；
* 若两个值的 value constructor 相同，则根据字段比较；

例如：

```Haskell
data Bool = False | True deriving (Ord)
```

因为 `False` 先于 `True` 被定义，所以 `Fasle` 值小于 `True`：

```Haskell
ghci> True `compare` False  
GT  
ghci> True > False  
True  
ghci> True < False  
False
```

而 `Maybe a` 中，`Nothing` 先于 `Just` 被定义，因此 `Nothing` 总是小于 `Just` 构造的值：

```Haskell
ghci> Nothing < Just 100  
True  
ghci> Nothing > Just (-49999)  
False  
ghci> Just 3 `compare` Just 2  
GT  
ghci> Just 100 > Just 50  
True  
```

两个 `Just` 构造的值比较时，`a` 必须也是 `Ord` 才行，因为函数类型不是 `Ord`，所以 `Just (* 3) > Just (* 2)` 是不合法的。

### `Enum` + `Bounded`

借助 algebraic data types + `Enum` + `Bounded` 可以很容易创建枚举。

```Haskell
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Staturday | Sunday
```

`Day` 类型的 7 个 value constructor 都没有参数，所以可以让 `Day` 变成 `Enum` 的一部分：

* `Enum` 是有前驱、后继的类型
* `Bounded` 是有最大、最小值的类型

```Haskell
data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Staturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)
```

因为 `Day` 是 `Show` 和 `Read` typeclass 的成员，因此可以将 `Day` 值转换为字符串，也可以将字符串转换为 `Day` 类型的值：

```Haskell
λ> show Sunday
"Sunday"
λ> read "Sunday" :: Day
Sunday
```

因为 `Day` 是 `Eq` typeclass 的成员，因此可以比较 `Day` 值是否相等：

```Haskell
λ> Sunday == Sunday
True
λ> Sunday == Monday
False
λ> Sunday /= Monday
True
```

`Day` 是 `Ord` typeclass 的成员，因此可以用 `>`、`>=`、`<`、`compare` 等比较 `Day` 值的大小顺序：

```Haskell
λ> Monday > Tuesday
False
λ> Monday < Tuesday
True
λ> Monday `compare` Tuesday
LT
```

`Day` 是 `Bounded` typeclass 的成员，因此可以获取其上下边界：

```Haskell
λ> minBound :: Day
Monday
λ> maxBound :: Day
Sunday
```

`Day` 是 `Enum` typeclass 的成员，因此可以获取其前驱、后继，更重要的是可创建 `Range`：

```Haskell
λ> succ Monday
Tuesday
λ> pred Monday
*** Exception: pred{Day}: tried to take `pred' of first tag in enumeration
CallStack (from HasCallStack):
  error, called at /Users/satansk/Projects/github/learn-you-a-haskell/ch7.hs:33:43 in main:Main
λ> [Monday .. Sunday]
[Monday,Tuesday,Wednesday,Thursday,Friday,Staturday,Sunday]
λ> [minBound .. maxBound] :: [Day]
[Monday,Tuesday,Wednesday,Thursday,Friday,Staturday,Sunday]
```

## 类型别名

前面说过 `[Char]` 和 `String` 等价，可以互换，因为 `String` 只是 `[Char]` 的类型别名：

```Haskell
type String = [Char]  
```

类型别名唯一作用：提升可读性。例如 `toUpperString` 将字符串全部转换为大写，它的类型既可以是 `toUpperString :: [Char] -> [Char]`，也可以是 `tpUpperString :: String -> String`，但后者明显更加可读。

再如：

```Haskell
phoneBook :: [(String,String)]  
phoneBook =      
    [("betty","555-2938")     
    ,("bonnie","452-2928")     
    ,("patsy","493-2928")     
    ,("lucille","205-2928")     
    ,("wendy","939-8282")     
    ,("penny","853-2492")     
    ]  
```

创建类型别名：

```Haskell
type PhoneBook = [(String, String)]
```

现在 `phoneBook` 类型就是 `PhoneBook` 了。

继续创建别名：

```Haskell
type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]
```

可读性更高！

函数 `inPhoneBook` 判断给定名字、电话是否在电话本中，由于前面定义了类型别名，该函数的类型非常清晰：

```Haskell
inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook
```

如果没有类型别名，`inPhoneBook` 类型为 `String -> String -> [(String,String)] -> Bool`，类型携带的信息非常少了。

### 类型参数 + 类型别名

类型别名也可以有类型参数，例如：

```Haskell
type AssocList k v = [(k, v)]
```

* 根据 key 查询对应 value 的函数的类型可以是：`(Eq k) => k -> AssocList k v -> Maybe v`
* `AssocList` 是一个 type constructor，接受两个 type 作为参数，产生一个 concrete type，例如 `AssocList Int String`

>Haskell 中，value 的类型只能是 concrete type，例如 `Maybe Int`，`Maybe` 是 type constructor，不存在 `Maybe` 类型的值。

Just like we can partially apply functions to get new functions, we can partially apply **type constructors** to get new type constructors from them!

例如：

```Haskell
type IntMap v = Map Int v
```

或：

```Haskell
type IntMap = Map Int
```

* 首先 `import Data.Map`

`Either a b` 有两个 type 参数：

```Haskell
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
```

* `Either` 有两个 value constructor

```Haskell
λ> Right 20
Right 20
λ> :t Right 20
Right 20 :: Num b => Either a b
λ> Left "error"
Left "error"
λ> :t Left "error"
Left "error" :: Either [Char] b
```

到目前为止，我们使用 `Maybe a` 表示可能失败的计算的结果，但失败时，只能获取一个 `Nothing`，没有关于失败的任何信息。

当函数只会由于一种原因失败时，这没问题，但若有多种原因都可以导致失败，则 `Nothing` 就太单薄了，此时可以用 `Either a b`。

```Haskell
data LockerState = Taken | Free deriving (Show, Eq)
type Code = String
type LockerMap = Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
  case Data.Map.lookup lockerNumber map of
    Nothing            -> Left $ "Locker number" ++ show lockerNumber ++ " doesn't exists!"
    Just (state, code) -> if state /= Taken
                          then Right code
                          else Left $ "Locker " ++ show lockerNumber ++ " is taken!"
```

使用：

```Haskell
λ> lockerLookup 10 (Data.Map.fromList [(10, (Taken, "10x"))])
Left "Locker 10 is taken!"
λ> lockerLookup 11 (Data.Map.fromList [(10, (Taken, "10x"))])
Left "Locker number11 doesn't exists!"
λ> lockerLookup 10 (Data.Map.fromList [(10, (Free, "10x"))])
Right "10x"
```

## 递归数据结构

### list

value constructor 的类型字段可以是本身 type，从而构成递归数据结构：

```Haskell
data List' a = Empty | Cons a (List' a)
  deriving (Show, Read, Eq, Ord)

λ> 1 `Cons` (2 `Cons` (3 `Cons` (4 `Cons` Empty)))
Cons 1 (Cons 2 (Cons 3 (Cons 4 Empty)))
```

* `Cons` 类似 `:`，而 `Empty` 类似 `[]`

通过反引号，可以让 `Cons` 变成中缀操作符，若函数全部由 **特殊字符** 组成，则函数自动中缀，value constructor 也是函数，所以：

```Haskell
data List' a = Empty | a :-: (List' a)
  deriving (Show, Read, Eq, Ord)

λ> 1 :-: Empty
1 :-: Empty
λ> 1 :-: (2 :-: Empty)
1 :-: (2 :-: Empty)
```

`:-:` 默认是左结合的，使用并不方便，使其变成右结合：

```Haskell
infixr 5 :-:
data List' a = Empty | a :-: (List' a)
  deriving (Show, Read, Eq, Ord)

λ> 1 :-: 2 :-: Empty
1 :-: (2 :-: Empty)
```

`infixr 5 :-:` 是 fixity 声明，决定 `:-:` 操作符的：

* 结合性：`infixr` 右结合，`infixl` 左结合；
* 优先级

每个操作符有定义一个数字，例如 `+` fixity 声明为 `infixl 6`，因此 `+` 优先级比 `:-:` 高，所以：

```Haskell
λ> 1 :-: 2 :-: 1 + 2 :-: Empty
1 :-: (2 :-: (3 :-: Empty))
```

list 拼接，标准库定义有 `++`：

```Haskell
infixr 5 ++
(++) :: [a] -> [a] -> [a]
[] ++ ys       = ys
(x : xs) ++ ys = x : (xs ++ ys)
```

将其改写为 `List'`：

```Haskell
infixr 5 .++
(.++) :: List' a -> List' a -> List' a
Empty .++ ys      = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

λ> let xs = 1 :-: 2 :-: Empty
λ> let ys = 3 :-: 4 :-: Empty
λ> xs .++ ys
1 :-: (2 :-: (3 :-: (4 :-: Empty)))
```

* `.++` 中，对 `Empty` 和 `(x :-: xs)` 进行模式匹配，因为模式匹配可用于 value constructor，所以是合法的；

实际上，可以为 `List' a` 实现所有 list 操作函数。

### binary search tree

二叉查找树很有用，`Data.Set` 和 `Data.Map` 是基于平衡二叉查找树构建的。

与 list 类似，tree 天然递归：

```Haskell
data Tree a = EmptyTree | Node a (Tree a) (Tree a)
  deriving (Show, Read, Eq)
```

* 假设 `Tree` 是二叉查找树，即左子树的值小于根节点，右子树的值大于根节点；

实现 `treeInsert` 函数，将一个值插入树中，并保持二叉查找树的性质：

```Haskell
singleton' :: a -> Tree a
singleton' x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => Tree a -> a -> Tree a
treeInsert EmptyTree x           = singleton' x
treeInsert (Node v left right) x
  | x == v    = Node v left right
  | x < v     = Node v (treeInsert left x) right
  | otherwise = Node v left (treeInsert right x)
```

实现 `treeElem`，检查指定元素是否在 tree 中存在：

```Haskell
treeElem :: (Ord a) => Tree a -> a -> Bool
treeElem EmptyTree _  = False
treeElem (Node v l r) x
  | x == v = True
  | x < v  = treeElem l x
  | x > v  = treeElem r x
```

使用 `foldl` 构建一棵树：

```Haskell
λ> let tree = Prelude.foldl treeInsert EmptyTree [8, 6, 4, 1, 7, 3, 5]
λ> tree
Node 8 (Node 6 (Node 4 (Node 1 EmptyTree (Node 3 EmptyTree EmptyTree)) (Node 5 EmptyTree EmptyTree)) (Node 7 EmptyTree EmptyTree)) EmptyTree
```

`treeElem`：

```Haskell
λ> treeElem tree 10
False
λ> treeElem tree 8
True
```

## Typeclass 102

前面通过 `deriving` 让 Haskell 编译器自动为 type 生成 typeclass instances，本节介绍如何手动创建 typeclass instance。

>如果一个 type 是某个 typeclass 的 instance，意味着该 type 可以使用 typeclass 中定义的函数。

`Eq` typeclass 定义可判等行为：

```Haskell
class Eq' a where
  (.==) :: a -> a -> Bool
  (./=) :: a -> a -> Bool
  x .== y = not (x ./= y)
  x ./= y = not (x .== y)
```

* `a` 实际为 `Eq` typeclass 的 instance type；

如果某 type 的两个值可以判断是否相等，则可以将该 type 变成 `Eq` typeclass 的实例。

假设有 ADT：

```Haskell
data TrafficLight = Red | Yellow | Green
```

将 `TrafficLight` 变成 `Eq` typeclass 的实例，需要：

```Haskell
instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False
```

>* `class` 用于定义新的 typeclass
>* `instance` 用于将特定 type 变成 typeclass 的实例（成员）

要让 `TrafficLight` 变成 `Show` typeclass 的实例，需要：

```Haskell
instance Show TrafficLight where
  show Red    = "Red light"
  show Yellow = "Yellow light"
  show Green  = "Green light"
```

验证：

```Haskell
λ> Red == Red
True
λ> Red == Yellow
False
λ> Red
Red light
λ> Yellow
Yellow light
λ> [Red, Yellow, Green]
[Red light,Yellow light,Green light]
```

`elem` 类型为：

```Haskell
λ> :t elem
elem :: (Eq a, Foldable t) => a -> t a -> Bool
```

因此 `TrafficLight` 可以使用 `elem`：

```Haskell
λ> Red `elem` [Red, Yellow, Green]
True
```

### typeclass 子类化

通过类型约束，可以让一个 typeclass 成为另一个 typeclass 的子类：

```Haskell
class (Eq a) => Num a where
```

它与：

```Haskell
class Num a where
```

基本相同，不过 `a` 必须是 `Eq` typeclass 的实例。

即，一个 type 想要成为 `Num` typeclass 的实例，首先必须是 `Eq` typeclass 的实例。

>子类，仅仅是类型约束的一种而已。

### `Maybe`

`Maybe` 是 type constructor，不是 concrete type，因此要让 `Maybe` 成为 `Eq` typeclass 的实例，需要：

```Haskell
instance (Eq m) => Eq (Maybe m) where
  Just x == Just y = x == y
  Nothing == Nothing = True
  _ == _ = False
```

* `(Eq m) => ` 才能让 `x == y` 合法

>:info 可以查看 typeclass 的所有实例，还可以查看某 ADT 属于哪些 typeclass，非常使用！

## A yes-no typeclass

```Haskell
class YesNo a where
  yesno :: a -> Bool
```

让很多 type 成为 `YesNo` typeclass 的实例：

```Haskell
instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _  = True

instance YesNo Bool where
  yesno = id

instance YesNo (Maybe a) where
  yesno Nothing = False
  yesno _       = True

instance YesNo (Tree a) where
  yesno EmptyTree = False
  yesno _         = True

instance YesNo TrafficLight where
  yesno Red = False
  yesno _   = True
```

借助 `YesNo` 实现 `if'`：

```Haskell
if' :: (YesNo a) => a -> b -> b -> b
if' yn x y = if (yesno yn) then x else y
```

测试：

```Haskell
    parse error (possibly incorrect indentation or mismatched brackets)
λ> if' [] 1 2
2
λ> if' EmptyTree 1 2
2
λ> if' Red 1 2
2
λ> if' Yellow 1 2
1
```

通过 `YesNo` typeclass，模式了动态语言特性。

## `Functor` typeclass

`Functor` typeclass 是一切可以 map 的 type 集合，list 即其中一员：

```Haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```

* `f` 是只有一个参数的 type constructor，而非 concrete type；

`List.map` 类型为 `(a -> b) -> [a] -> [b]`，与 `fmap` 非常类似，可以轻易把 `List` 变成 `Functor` 的实例：

```Haskell
instance Functor' [] where
  fmap' = Prelude.map
```

因此，对于 list 而言，`fmap` 和 `map` 完全等价：

```Haskell
λ> fmap' (+ 1) [1, 2, 3]
[2,3,4]
λ> Prelude.map (+ 1) [1, 2, 3]
[2,3,4]
```

`Maybe` 也可视为 `Functor'` 的实例：

```Haskell
instance Functor' Maybe where
  fmap' f Nothing  = Nothing
  fmap' f (Just x) = Just $ f x
```

测试：

```Haskell
λ> fmap' (+ 1) (Just 10)
Just 11
λ> fmap' (+ 1) Nothing
Nothing
```

`Tree` 也可视为 `Functor'` 实例：

```Haskell
instance Functor' Tree where
  fmap' _ EmptyTree    = EmptyTree
  fmap' f (Node v l r) = Node (f v) (fmap' f l) (fmap' f r)
```

测试：

```Haskell
λ> fmap' (+ 1) EmptyTree
EmptyTree
λ> fmap' (+ 1) $ Prelude.foldl treeInsert EmptyTree [1, 2, 3]
Node 2 EmptyTree (Node 3 EmptyTree (Node 4 EmptyTree EmptyTree))
```

`Either a b` 也可以成为 `Functor'` 实例：

```Haskell
instance Functor' (Either a) where
  fmap' _ (Left x)  = Left x
  fmap' f (Right x) = Right $ f x
```

* `Functor'` 需要一个参数的 type constructor，而 `Either a b` 有两个，所以需要固定其中一个；

>记住，type constructor 也可以部分应用，`Either a` 返回一个新的、接受一个类型的 type constructor。

测试：

```Haskell
λ> fmap' (+ 1) $ Right 10
Right 11
λ> fmap' (+ 1) $ Left 10
Left 10
```

## Kind 与无名类型

value 有 type：

```Haskell
λ> :t 10
10 :: Num t => t
λ> :t fmap
fmap :: Functor f => (a -> b) -> f a -> f b
```

* `fmap` 函数也是值

type 有 kind：

```Haskell
λ> :k Int
Int :: *
```

* `*` 表示该 type 为 concrete type；

`Maybe`：

```Haskell
λ> :k Maybe
Maybe :: * -> *
```

`* -> *` 表示接受一个 concrete type，并产生一个 concrete type；

```Haskell
λ> :k Maybe String
Maybe String :: *
```

* `Maybe String` 的 kind 为 `*`，是一个 concrete type；



```
λ> :k Either
Either :: * -> * -> *
λ> :k Functor
Functor :: (* -> *) -> Constraint
```