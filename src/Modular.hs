module Modular where

------
-- Defining modular arithmetic types
------

newtype Mod2 = M2 Integer deriving (Eq)

instance Num Mod2 where
  (M2 x) + (M2 y) = fromInteger (x + y)
  (M2 x) * (M2 y) = fromInteger (x * y)
  negate = id
  abs = id
  signum = const (M2 1)
  fromInteger n = M2 (n `mod` 2)

instance Show Mod2 where
  show (M2 x) = show x

newtype Mod3 = M3 Integer deriving (Eq)

instance Num Mod3 where
  (M3 x) + (M3 y) = fromInteger (x + y)
  (M3 x) * (M3 y) = fromInteger (x * y)
  negate (M3 x) = fromInteger (- x)
  abs = id
  signum = const (M3 1)
  fromInteger n = M3 (n `mod` 3)

instance Show Mod3 where
  show (M3 x) = show x

newtype Mod4 = M4 Integer deriving (Eq)

instance Num Mod4 where
  (M4 x) + (M4 y) = fromInteger (x + y)
  (M4 x) * (M4 y) = fromInteger (x * y)
  negate (M4 x) = fromInteger (- x)
  abs = id
  signum = const (M4 1)
  fromInteger n = M4 (n `mod` 4)

instance Show Mod4 where
  show (M4 x) = show x

------
-- Utilities
------

-- These are just to work with the type system

class ToIntegral a where
  toIntegral :: (Integral b) => a -> b

instance ToIntegral Mod2 where
  toIntegral (M2 x) = fromInteger x

instance ToIntegral Mod3 where
  toIntegral (M3 x) = fromInteger x

instance ToIntegral Mod4 where
  toIntegral (M4 x) = fromInteger x
