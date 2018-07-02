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











