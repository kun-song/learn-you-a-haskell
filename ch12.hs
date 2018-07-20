
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

