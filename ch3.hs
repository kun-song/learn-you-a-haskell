{-# LANGUAGE OverloadedStrings #-}

lucky :: Int -> String
lucky 7 = "Luky, number 7!"
lucky _ = "Sorry, you're out of luck."

fact :: Int -> Int
fact 0 = 1
fact n = n * fact (pred n)

addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (a, b) (c, d) = (a + c, b + d)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, y, _) = y

third :: (a, b, c) -> c
third (_, _, z) = z

head' :: [a] -> a
head' []      = error "head on []!"
head' (x : _) = x

firstLetter :: String -> String
firstLetter xs @ (x : _) = "The first letter of " ++ xs ++ " is " ++ [x]

max' :: (Ord a) => a -> a -> a
max' a b
  | a > b     = a
  | otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
  | a == b    = EQ
  | a <= b    = LT
  | otherwise = GT

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
  where (f : _) = firstname
        (l : _) = lastname

calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi h w | (h, w) <- xs]
  where bmi weight height = weight / height ^ 2

cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r ^ 2
  in  sideArea + 2 * topArea

calcBmis' :: [(Double, Double)] -> [Double]
calcBmis' xs = [bmi | (h, w) <- xs, let bmi = w / h ^ 2, bmi > 25.0]

