import Data.Map

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

type Name = String
type PhoneNumber = String
type PhoneBook = [(Name, PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool
inPhoneBook name number book = (name, number) `elem` book

type AssocList k v = [(k, v)]

type IntMap = Map Int

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

infixr 5 :-:
data List' a = Empty | a :-: (List' a)
  deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List' a -> List' a -> List' a
Empty .++ ys      = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

data Tree a = EmptyTree | Node a (Tree a) (Tree a)
  deriving (Show, Read, Eq)

singleton' :: a -> Tree a
singleton' x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => Tree a -> a -> Tree a
treeInsert EmptyTree x           = singleton' x
treeInsert (Node v left right) x
  | x == v    = Node v left right
  | x < v     = Node v (treeInsert left x) right
  | otherwise = Node v left (treeInsert right x)

treeElem :: (Ord a) => Tree a -> a -> Bool
treeElem EmptyTree _  = False
treeElem (Node v l r) x
  | x == v = True
  | x < v  = treeElem l x
  | x > v  = treeElem r x

class Eq' a where
  (.==) :: a -> a -> Bool
  (./=) :: a -> a -> Bool
  x .== y = not (x ./= y)
  x ./= y = not (x .== y)

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red    = "Red light"
  show Yellow = "Yellow light"
  show Green  = "Green light"

class YesNo a where
  yesno :: a -> Bool

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

if' :: (YesNo a) => a -> b -> b -> b
if' yn x y = if (yesno yn) then x else y

