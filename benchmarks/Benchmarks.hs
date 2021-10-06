import Test.Tasty.Bench

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import Linear hiding (trace)

import Trace.Light
import Trace.Material
import Trace.Scene
import Trace.Surface

main :: IO ()
main =
  defaultMain
    [ bgroup
        "trace"
        [ bench "small scene" $ nf (trace smallScene) (rayAt 0 0)
        , bench "large scene" $ nf (trace largeScene) (rayAt 0 0)
        ]
    , bgroup
        "intersections"
        [ bench "small scene" $ nf (flip intersections smallScene) (rayAt 0 0)
        , bench "small scene" $ nf (flip intersections largeScene) (rayAt 0 0)
        ]
    ]

smallScene :: Scene
smallScene =
  Scene
    { background = V3 0 0 0
    , objects =
        Vector.fromList
          [ sphere (V3 1.8 (-0.5) 0) 0.8 (Material{colour = V3 1 0 0})
          , sphere (V3 1.8 0.3 (-0.5)) 0.5 (Material{colour = V3 0 0 1})
          ]
    , ambient = 0.4
    , lights = Vector.fromList [Light{position = V3 1 (-0.5) 1.2, intensity = 0.8}]
    }

largeScene :: Scene
largeScene =
  Scene
    { background = V3 0 0 0
    , objects =
        Vector.fromList
          [ sphere (V3 (x + 3) y z) 0.1 (Material{colour = 1})
          | (x, y, z) <- (,,) <$> range <*> range <*> range
          ]
    , ambient = 0.2
    , lights =
        Vector.fromList
          [ Light{position = V3 2 (-1) 0.75, intensity = V3 1 0 0}
          , Light{position = V3 2 1 0.75, intensity = V3 0 1 0}
          , Light{position = V3 2 0 (-0.75), intensity = V3 0 0 1}
          ]
    }
 where
  range = [-2, -1.5 .. 2]
