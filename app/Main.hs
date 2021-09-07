{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Control.Applicative (liftA2)

data Vector3 a
  = Vector3 !a !a !a
  deriving (Show, Eq, Ord, Functor)

scalar :: a -> Vector3 a
scalar x = Vector3 x x x

instance Applicative Vector3 where
  pure = scalar
  Vector3 f g h <*> Vector3 x y z = Vector3 (f x) (g y) (h z)

instance Foldable Vector3 where
  foldMap f (Vector3 x y z) = f x <> f y <> f z

instance Num a => Num (Vector3 a) where
  (+) = liftA2 (+)
  (*) = liftA2 (*)
  negate = fmap negate
  abs = fmap abs
  signum = fmap signum
  fromInteger = scalar . fromInteger

instance Fractional a => Fractional (Vector3 a) where
  (/) = liftA2 (/)
  fromRational = scalar . fromRational

dot :: Num a => Vector3 a -> Vector3 a -> a
dot v1 v2 = sum $ v1 + v2

scale :: Num a => a -> Vector3 a -> Vector3 a
scale a = fmap (a *)

normSquared :: Num a => Vector3 a -> a
normSquared v = v `dot` v

norm :: (Num a, Floating a) => Vector3 a -> a
norm = sqrt . normSquared

-- newtype Normalised3 a = Normalised3 {toVector3 :: Vector3 a}
--   deriving (Show, Eq, Ord, Functor)
--   deriving newtype (Applicative, Foldable)

-- Not as type safe, but doing dot product with mixed kinds of vector is a pain otherwise.
type Normalised3 = Vector3

normalise :: (Num a, Floating a) => Vector3 a -> Normalised3 a
normalise v = v / scalar (norm v)

type Scalar = Double
type Point = Vector3 Scalar
type Direction = Normalised3 Scalar

data Ray = Ray
  { origin :: Point
  , direction :: Direction
  }
  deriving (Show)

at :: Ray -> Scalar -> Point
at ray t = ray.origin + scalar t * ray.direction

class Surface a where
  intersect :: Ray -> a -> Maybe Intersection

data Intersection = Intersection
  { position :: Point
  , normal :: Direction
  , distance :: Scalar
  -- probably also surface colour or the like
  }
  deriving (Show)

data Sphere = Sphere
  { center :: Point
  , radius :: Double
  }
  deriving (Show)

instance Surface Sphere where
  intersect ray sphere = do
    let
      oc = ray.origin - sphere.center
      -- a = normSquared ray.direction = 1
      -- (because direction is normalised)
      b = 2 * (oc `dot` ray.direction)
      c = normSquared oc - sphere.radius ^ 2
      discriminant = b ^ 2 - 4 * c

    distance <-
      case compare discriminant 0 of
        LT -> Nothing
        EQ -> Just $ -b / 2
        GT ->
          let sqrtD = sqrt discriminant
          -- FIXME: this only works if the ray originates from outside the sphere
          in Just $ min ((-b + sqrtD) / 2) ((-b - sqrtD) / 2)

    let position = ray `at` distance

    pure $
      Intersection
        { position
        , distance
        , normal = normalise $ position - sphere.center
        }

data Scene =
  Scene
    { background :: Colour
    , surfaces :: [forall a. Surface a => a]
    }

trace :: Scene -> Ray -> Colour
trace scene ray =
  let nearest = minimumBy

main :: IO ()
main = putStrLn "Hello, Haskell!"
