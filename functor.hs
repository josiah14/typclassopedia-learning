{-# LANGUAGE NoImplicitPrelude #-}

import Prelude(Show, (.))

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
