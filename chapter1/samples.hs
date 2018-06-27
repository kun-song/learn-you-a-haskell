{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- 函数定义
doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe y

-- if-then-else 两个分支不可缺少
doubleSmallNumber x = if x > 100
                      then x
                      else x * 2

doubleSmallNumber' x = (if x > 100 then x else x * 2) + 1

song'kun = "it's me Song Kun"

boomBang xs = [if x > 10 then "BANG" else "BOOM" | x <- xs, odd x]

length' :: [a] -> Int
length' xs = sum [1 | _ <- xs]

removeNoUppercase :: String -> String
removeNoUppercase s = [x | x <- s, x `elem` ['A' .. 'Z']]

removeOdd :: [[Int]] -> [[Int]]
removeOdd xss = [[x | x <- xs, even x] | xs <- xss]

fst' :: (a, b) -> a
fst' (x, _) = x

snd' :: (a, b) -> b
snd' (_, y) = y

fact :: Integer -> Integer
fact n = product [1 .. n]

circumference :: Float -> Float
circumference r = 2 * pi * r

circumference' :: Double -> Double
circumference' r = 2 * pi * r


