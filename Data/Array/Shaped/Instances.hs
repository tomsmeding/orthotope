--
-- Numeric instances that can be useful, but might not be what you want.
--
{-# OPTIONS_GHC -Wno-orphans #-}
module Data.Array.Shaped.Instances where
import Data.Array.Shaped

instance (Shape sh, Num a) => Num (Array sh a) where
  (+) = zipWithA (+)
  (-) = zipWithA (-)
  (*) = zipWithA (*)
  negate = mapA negate
  abs = mapA abs
  signum = mapA signum
  fromInteger = constant . fromInteger

instance (Shape sh, Fractional a) => Fractional (Array sh a) where
  (/) = zipWithA (/)
  recip = mapA recip
  fromRational = constant . fromRational

instance (Shape sh, Floating a) => Floating (Array sh a) where
  pi = constant pi
  exp = mapA exp
  log = mapA log
  sqrt = mapA sqrt
  (**) = zipWithA (**)
  logBase = zipWithA logBase
  sin = mapA sin
  cos = mapA cos
  tan = mapA tan
  asin = mapA asin
  acos = mapA acos
  atan = mapA atan
  sinh = mapA sinh
  cosh = mapA cosh
  tanh = mapA tanh
  asinh = mapA asinh
  acosh = mapA acosh
  atanh = mapA atanh

-- Real cannot be implemented, but is a superclass of Integral.
instance (Shape sh, Real a) => Real (Array sh a) where
  toRational _ = error "toRational of an Array"

-- Enum cannot be implemented, but is a superclass of Integral.
instance (Shape sh, Enum a) => Enum (Array sh a) where
  toEnum _ = error "toEnum of an Array"
  fromEnum _ = error "fromEnum of an Array"

-- Integral can only be partially implemented
instance (Shape sh, Integral a) => Integral (Array sh a) where
  quot = zipWithA quot
  rem = zipWithA rem
  div = zipWithA div
  mod = zipWithA mod
  quotRem x y = (quot x y, rem x y)
  divMod x y = (div x y, mod x y)
  toInteger _ = error "toInteger of an Array"
