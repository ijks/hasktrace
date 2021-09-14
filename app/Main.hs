{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Codec.Picture
import Control.Applicative (liftA2)
import Data.Foldable (minimumBy)
import Data.Function (on, (&))
import Data.Maybe (mapMaybe)

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
dot v1 v2 = sum $ v1 * v2

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

lerp :: Num a => a -> a -> a -> a
lerp t u v = t * v + (1 - t) * u

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

aimedAt :: Point -> Point -> Ray
aimedAt from to =
  Ray
    { origin = from
    , direction = normalise $ to - from
    }

at :: Ray -> Scalar -> Point
at ray t = origin ray + scalar t * direction ray

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
    let oc = origin ray - center sphere
        -- a = normSquared ray.direction = 1
        -- (because direction is normalised)
        b = 2 * (oc `dot` direction ray)
        c = normSquared oc - radius sphere ^ 2
        discriminant = b ^ 2 - 4 * c

    distance <-
      case compare discriminant 0 of
        LT -> Nothing
        EQ -> Just $ - b / 2
        GT ->
          let sqrtD = sqrt discriminant
           in -- FIXME: this only works if the ray originates from outside the sphere
              Just $ min ((- b + sqrtD) / 2) ((- b - sqrtD) / 2)

    let position = ray `at` distance

    pure $
      Intersection
        { position
        , distance
        , normal = normalise $ position - center sphere
        }

type Colour = Float

data SomeSurface where
  Surface :: Surface a => a -> SomeSurface

instance Surface SomeSurface where
  intersect ray (Surface s) = intersect ray s

data Scene = Scene
  { background :: Colour
  , surfaces :: [SomeSurface]
  }

trace :: Scene -> Ray -> Colour
trace scene ray =
  let intersections =
        surfaces scene
          & mapMaybe (intersect ray)
   in if null intersections
        then background scene
        else 1.0

-- intersections & minimumBy (compare `on` (.distance)) & (.distance)

-- TODO: camera record
rayAt :: Double -> Double -> Ray
rayAt screenX screenY =
  let depth = 1.0
      y = lerp screenX (-1) 1
      z = lerp screenY 1 (-1)
      rayTarget = Vector3 depth y z
   in aimedAt (scalar 0) rayTarget

testScene =
  Scene
    { background = 0.0
    , surfaces = [Surface $ Sphere{center = Vector3 2 0 0, radius = 1}]
    }

main :: IO ()
main = do
  let (width, height) = (512, 512)
  let image =
        generateImage
          ( \pixelX pixelY ->
              let x = fromIntegral pixelX / fromIntegral width
                  y = fromIntegral pixelY / fromIntegral height
               in trace testScene (rayAt x y)
          )
          width
          height

  savePngImage "test.png" (ImageYF image)
