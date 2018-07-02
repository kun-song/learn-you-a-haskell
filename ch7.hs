
data Point = Point Float Float deriving (Show)

data Shape = Circle Point Float | Rectangle Point Point
  deriving (Show)

surface :: Shape -> Float
surface (Circle _  r) = pi * r ^ 2
surface (Rectangle (Point x1 y1) (Point x2 y2)) = (abs $ x1 - x2) * (abs $ y1 - y2)

nudge :: Shape -> Float -> Float -> Shape
nudge (Circle (Point x y) r) a b = Circle (Point (x + a) (y + b)) r
nudge (Rectangle (Point x1 y1) (Point x2 y2)) a b = Rectangle (Point (x1 + a) (y1 + b)) (Point (x2 + a) (y2 + b))

data Vector a = Vector a a a deriving (Show)

vPlus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector a b c) `vPlus` (Vector d e f) = Vector (a + d) (b + e) (c + f)

vectMulti :: (Num a) => Vector a -> a -> Vector a
(Vector a b c) `vectMulti` m = Vector (a * m) (b * m) (c * m)

scalarMulti :: (Num t) => Vector t -> Vector t -> t
(Vector a b c) `scalarMulti` (Vector d e f) = a * d + b * e + c * f

data Student = Student { firstName :: String
                       , lastName :: String
                       , age :: Int
                       }
               deriving (Eq, Show, Read)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Staturday | Sunday
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

