
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
