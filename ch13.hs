
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing _  = Nothing
appluMaybe (Just x) f = f x

class Monad' m where
  return' :: a -> m a
  (>>=!) :: m a -> (a -> m b) -> m b
  (>>!) :: m a -> m b -> m b
  x >>! y = x >>=! \_ -> y

  fail' :: String -> m a
  fail' msg = error msg

instance Monad' Maybe where
  return' = Just
  Just x >>=! f = f x
  Nothing >>=!_ = Nothing

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (l, r)
  | abs ((l + n) - r) < 4 = Just (l + n, r)
  | otherwise             = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (l, r)
  | abs (l - (r + n)) < 4 = Just (l, r + n)
  | otherwise             = Nothing

banana :: Pole -> Maybe Pole
banana _ = Nothing

foo :: Maybe String
foo = do
        x <- Just 11
        y <- Just "!"
        Just $ show x ++ y

routine :: Maybe Pole
routine =
  do
    start <- return (0, 0)
    first <- landLeft 2 start
    second <- landRight 3 first
    landLeft 1 second

