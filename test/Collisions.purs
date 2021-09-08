module Test.Main where

import Data.Array
import NoControl.Engine
import NoControl.Engine.Collisions
import Prelude
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Class.Console as Effect
import Main.Types (ObjectType(..))
import Test.Assert (assert)

o position =
  { position
  , energy: { x: 3.0, y: -2.0 }
  , characteristics: { maxFallSpeed: 10.0, bounceability: 0.0 }
  , type: None
  }

--show o = o.x + "-" + (o.x + o.width) + "/" + o.y + "-" + o.y + o.height
formatObjects arr = (map (map (\a -> a.position)) arr)

-- showObjects = logShow formatObjects
showObjectsLength = length >>> logShow

horizontallyOverlappingObjects =
  [ o ({ x: 0.0, y: 0.0, width: 21.0, height: 10.0 })
  , o ({ x: 20.0, y: 999.0, width: 10.0, height: 10.0 })
  ]

nonOverlappingObjects =
  [ o ({ x: 21.0, y: 999.0, width: 10.0, height: 10.0 })
  , o ({ x: 0.0, y: 0.0, width: 21.0, height: 10.0 })
  ]

overlappingObjects =
  [ o ({ x: 0.0, y: 0.0, width: 21.0, height: 21.0 })
  , o ({ x: 20.0, y: 20.0, width: 10.0, height: 10.0 })
  ]

main :: Effect Unit
main = do
  assert
    -- HorizontalOverlap: horizontally overlapping objects are put into one segment
    ( length
        ( segmentBy horizontalOverlap
            ( sortByX horizontallyOverlappingObjects
            )
        )
        == 1
    )
  assert
    -- HorizontalOverlap: non-overlapping objects are left in two separate segments
    ( length
        ( segmentBy horizontalOverlap
            ( sortByX nonOverlappingObjects
            )
        )
        == 2
    )
  assert
    -- FullOverlap: horizontally overlapping objects are left in separate segments
    ( length
        ( overlappingSegments
            ( sortByX horizontallyOverlappingObjects
            )
        )
        == 2
    )
  assert
    -- FullOverlap: overlapping objects are in one segment
    ( length
        ( overlappingSegments
            ( sortByX overlappingObjects
            )
        )
        == 1
    )
