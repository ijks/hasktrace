module Main where

import Codec.Picture
import Control.Applicative (liftA2)
import Data.Foldable (minimumBy)
import Data.Function (on, (&))
import Data.Maybe (mapMaybe)
import Linear hiding (distance, lerp, trace)

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

type Scalar = Float
type Point = V3 Scalar
type Direction = N3 Scalar

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
at ray t = origin ray + pure t * direction ray

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
        else
          let intersection = minimumBy (compare `on` distance) intersections
           in min 1 (distance intersection * 0.4)

-- intersections & minimumBy (compare `on` (.distance)) & (.distance)

-- TODO: camera record
rayAt :: Scalar -> Scalar -> Ray
rayAt screenX screenY =
  let depth = 1.0
      y = lerp screenX (-1) 1
      z = lerp screenY 1 (-1)
      rayTarget = V3 depth y z
   in aimedAt 0 rayTarget

testScene :: Scene
testScene =
  Scene
    { background = 0.0
    , surfaces =
        [ Surface $ Sphere{center = V3 1.8 (-0.5) 0, radius = 0.8}
        , Surface $ Sphere{center = V3 1.8 0.5 0, radius = 0.8}
        ]
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
