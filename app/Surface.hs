{-# LANGUAGE RankNTypes #-}

module Surface where

import Linear

import Geometry

data Intersection = Intersection
  { position :: Point
  , normal :: Direction
  , distance :: Scalar
  -- probably also surface colour or the like
  }
  deriving (Show)

class Surface a where
  intersect :: Ray -> a -> Maybe Intersection

data Sphere = Sphere
  { center :: Point
  , radius :: Scalar
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

data SomeSurface where
  Surface :: Surface a => a -> SomeSurface

instance Surface SomeSurface where
  intersect ray (Surface s) = intersect ray s
