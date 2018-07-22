import qualified Data.Foldable as F

class Monoid' m where
  mempty' :: m
  mappend' :: m -> m -> m
  mconcat' :: [m] -> m
  mconcat' = foldr mappend' mempty'

instance Monoid' [a] where
  mempty' = []
  mappend' = (++)


newtype Product' a = Product' { getProduct' :: a } deriving (Eq, Ord, Read, Show, Bounded)

instance (Num a) => Monoid (Product' a) where
  mempty = Product' 1
  Product' x `mappend` Product' y = Product' $ x * y


newtype Sum' a = Sum' { getSum' :: a } deriving (Eq, Ord, Read, Show, Bounded)

instance (Num a) => Monoid (Sum' a) where
  mempty = Sum' 0
  Sum' x `mappend` Sum' y = Sum' $ x + y


newtype Any' = Any' { getAny' :: Bool } deriving (Show)

instance Monoid Any' where
  mempty = Any' False
  Any' x `mappend` Any' y = Any' $ x || y


newtype All' = All' { getAll' :: Bool } deriving (Show)

instance Monoid All' where
  mempty = All' True
  All' x `mappend` All' y = All' $ (x && y)


instance Monoid' a => Monoid' (Maybe a) where
  mempty' = Nothing
  Nothing `mappend'` m = m
  m `mappend'` Nothing = m
  Just x `mappend'` Just y = Just $ x `mappend'` y


newtype First a = First { getFirst :: Maybe a } deriving (Eq, Show)

instance Monoid (First a) where
  mempty = First Nothing
  First (Just x) `mappend` _ = First $ Just x
  First Nothing `mappend` f  = f


newtype Last a = Last { getLast :: Maybe a } deriving Show

instance Monoid (Last a) where
  mempty = Last Nothing
  _ `mappend` Last (Just x) = Last (Just x)
  f `mappend` Last Nothing  = f

data Tree a = Empty | Node a (Tree a) (Tree a) deriving Show

instance F.Foldable Tree where
  foldMap f Empty = mempty
  foldMap f (Node x l r) = F.foldMap f l `mappend` f x `mappend` F.foldMap f r
