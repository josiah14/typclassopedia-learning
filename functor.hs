{-# LANGUAGE NoImplicitPrelude #-}

import Prelude(Show, (.), Int, map)

class Functor f where
  fmap :: (a -> b) -> f a -> f b

instance Functor [] where
  fmap _ []     = []
  fmap g (x:xs) = g x : fmap g xs

data Maybe a = Just a | Nothing
  deriving (Show)

instance Functor Maybe where
  fmap _ Nothing  = Nothing
  fmap g (Just x) = Just (g x)

data Either e a = Left e | Right a
  deriving (Show)

instance Functor (Either e) where
  fmap _ (Left x)  = Left x
  fmap g (Right x) = Right (g x)

instance Functor ((->) e) where
  fmap = (.)

instance Functor ((,) e) where
  fmap g (a, b) = (a, g b)

data Pair a = Pair a a

instance Functor Pair where
  fmap g (Pair x y) = Pair (g x) (g y)

data ITree a = Leaf (Int -> a) | Node [ITree a]

instance Functor ITree where
  fmap g (Node trees) = Node (map (fmap g) trees)
  fmap g (Leaf h)     = Leaf (g . h)

composeFunctors :: (Functor f0, Functor f1) => (a -> b) -> f0 (f1 a) -> f0 (f1 b)
composeFunctors = fmap . fmap

-- An examlpe of a non functor of type * -> * is ...
data K a = K (a -> Int)

-- Example of a functor that satisfies the 'function composition equivalence' functor law, but not the 'id' functor law.
data BogusFunctor a = Bog a | Us a deriving Show

instance Functor BogusFunctor where
  -- This changes the context and therefore breaks the identity law, even though it still satisfies the composition law.
  fmap g (Bog x) = Us (g x)
  fmap g (Us x) = Us (g x)

-- Evil functor instance
instance Functor [] where
  fmap _ []     = []
  fmap g (x:xs) = g x : g x : fmap g xs

-- The above functor instance violates both functor laws.
-- It violates the identity law because:
--   fmap id [1,2,3] => [1,1,2,2,3,3]
-- It violates the composition identity because:
--   fmap ((/ 2) . read) ["1", "2"] => [0.5, 0.5, 1.0, 1.0], but:
--   ((fmap (/2)) . (fmap read)) ["1", "2"] => [0.5, 0.5, 0.5, 0.5, 1.0, 1.0, 1.0, 1.0]
