import Data.Char
import Data.List
import Control.Applicative

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

data CMaybe a = CNothing | CJust Int a deriving (Show)

instance Functor CMaybe where
  fmap f CNothing    = CNothing
  fmap f (CJust c x) = CJust (c + 1) (f x)

class (Functor f) => Applicative' f where
  pure' :: a -> f a
  (<*!>) :: f (a -> b) -> f a -> f b

instance Applicative' IO where
  pure' = return
  x <*!> y = do
               f <- x
               a <- y
               return $ f a

myAction1 :: IO String
myAction1 =
  do
    a <- getLine
    b <- getLine
    return $ a ++ b

myAction2 :: IO String
myAction2 = (++) <$> getLine <*> getLine

instance Applicative' ((->) r) where
  pure' x = (\_ -> x)
  f <*!> g = \x -> f x $ g x

instance Applicative' ZipList where
  pure' x = ZipList (repeat x)
  ZipList fs <*!> ZipList xs = ZipList $ zipWith (\f x -> f x) fs xs

liftA2' :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2' f fa fb = f <$> fa <*> fb

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' []       = pure []
sequenceA' (fa : fas) = (:) <$> fa <*> sequenceA' fas

sequenceA'' :: (Applicative f) => [f a] -> f [a]
sequenceA'' []         = pure []
sequenceA'' (fa : fas) = liftA2 (:) fa (sequenceA'' fas)

sequenceA''' :: (Applicative f) => [f a] -> f [a]
sequenceA''' = foldr (liftA2 (:)) (pure [])

main :: IO ()
main =
  do x <- myAction2
     putStrLn x
