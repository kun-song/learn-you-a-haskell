import Data.Char
import Data.List

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b

instance Functor' IO where
  fmap' f action = do a <- action
                      return $ f a
-- IO a 是 Functor 实例
play1 :: IO ()
play1 = do line <- getLine
           let line' = reverse line
           putStrLn $ "You said " ++ line' ++ " backwards!"
           putStrLn $ "Yes, you really said " ++ line

play2 :: IO ()
play2 = do line <- fmap' reverse getLine
           putStrLn $ "You said " ++ line ++ " backwards!"
           putStrLn $ "Yes, you really said " ++ line

play3 :: IO ()
play3 = do line <- fmap' (intersperse '-' . reverse . map toUpper) getLine
           putStrLn line


-- r -> 即 (->) r 也是 Functor 实例
-- f :: a -> b, g :: r -> a, fmap' 结果为 r -> b
instance Functor' ((->) r) where
  fmap' = (.)

main :: IO ()
main = play3

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap f CNothing    = CNothing
  fmap f (CJust c x) = CJust (c + 1) (f x)
