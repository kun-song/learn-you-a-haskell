
maximum' :: Ord a => [a] -> a
maximum' []       = error "maximum' on []"
maximum' [x]      = x
maximum' (x : xs) = x `max` maximum' xs

replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n - 1) x

take' :: Int -> [a] -> [a]
take' _ []         = []
take' n _ | n <= 0 = []
take' n (x : xs)   = x : take' (n - 1) xs

reverse' :: [a] -> [a]
reverse' []       = []
reverse' (x : xs) = reverse' xs ++ [x]

repeat' :: a -> [a]
repeat' x = x : repeat' x

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

elem' :: Eq a => a -> [a] -> Bool
elem' _ []      = False
elem' x (h : t) = x == h || x `elem'` t

quickSort :: (Ord a) => [a] -> [a]
quickSort []  = []
quickSort (x : xs) = quickSort [z | z <- xs, z <= x] ++ [x] ++ quickSort [z | z <- xs, z > x]

compareWithHundred :: Int -> Ordering
compareWithHundred x = compare 100 x

compareWithHundred' :: Int -> Ordering
compareWithHundred' = compare 100

dividedByTen :: (Floating a) => a -> a
dividedByTen = (/ 10)

isUpperCase :: Char -> Bool
isUpperCase = (`elem` ['A' .. 'Z'])

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _              = []
zipWith' _ _ []              = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f = g
  where g b a = f a b

flip'' :: (a -> b -> c) -> b -> a -> c
flip'' f b a = f a b

map' :: (a -> b) -> [a] -> [b]
map' _ []       = []
map' f (x : xs) = f x : map' f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ []  = []
filter' p (x : xs)
  | p x       = x : filter' p xs
  | otherwise = filter' p xs

chain :: Integer -> [Integer]
chain 1 = [1]
chain n
  | even n = n : chain (n `div` 2)
  | odd n  = n : chain (n * 3 + 1)

sum' :: (Num a) => [a] -> a
sum' = foldl (+) 0

map2 :: (a -> b) -> [a] -> [b]
map2 f xs = foldr (\h t -> f h : t) [] xs

map3 :: (a -> b) -> [a] -> [b]
map3 f xs = foldl (\t h -> t ++ [f h]) [] xs

elem1 :: (Eq a) => a -> [a] -> Bool
elem1 x xs = foldr (\z acc -> if x == z then True else acc) False xs

maximum1 :: (Ord a) => [a] -> a
maximum1 = foldl1 max

reverse1 :: [a] -> [a]
reverse1 = foldl (\acc h -> h : acc) []

reverse2 :: [a] -> [a]
reverse2 = foldl (flip (:)) []

product1 :: (Num a) => [a] -> a
product1 = foldl (*) 1

filter1 :: (a -> Bool) -> [a] -> [a]
filter1 p = foldr (\h acc -> if p h then h : acc else acc) []

last1 :: [a] -> a
last1 = foldl1 (\_ x -> x)

head1 :: [a] -> a
head1 = foldr1 (\x _ -> x)

and' :: [Bool] -> Bool
and' = foldr (&&) True

and'' :: [Bool] -> Bool
and'' = foldl (&&) True
