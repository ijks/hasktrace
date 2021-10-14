module Trace.Geometry where

import Linear hiding (lerp)

type Scalar = Float
type Point = V3 Scalar
type Direction = N3 Scalar

lerp :: Num a => a -> a -> a -> a
lerp t u v = t * v + (1 - t) * u

normSquared :: Num a => V3 a -> a
normSquared = quadrance

-- newtype Normalised3 a = Normalised3 {toV3 :: V3 a}
--   deriving (Show, Eq, Ord, Functor)
--   deriving newtype (Applicative, Foldable)

-- Type to keep track of normalised (i.e. norm = 1) vectors.
-- Not as type safe, but doing dot product with mixed kinds of vector is a pain otherwise.
type N3 = V3

normalise :: Floating a => V3 a -> N3 a
normalise = signorm

data Ray = Ray
  { origin :: !Point
  , direction :: !Direction
  }
  deriving (Show)

aimedAt :: Point -> Point -> Ray
aimedAt from to =
  Ray
    { origin = from
    , direction = normalise $ to - from
    }

at :: Ray -> Scalar -> Point
at ray t = origin ray + pure t * direction ray
