module Test.Main where

import Data.Array
import NoControl.Engine
import NoControl.Engine.Collisions
import Prelude
import Debug (spy, trace)
import Effect (Effect)
import Effect.Class.Console (log, logShow)
import Effect.Class.Console as Effect
import Main.Types (ObjectType(..))
import Test.Assert (assert)

o position =
  { position
  , energy: { x: 3.0, y: -2.0 }
  , characteristics: { maxFallSpeed: 10.0, bounceability: 0.0, color: "red", distance: 1.0 }
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

otherObjects =
  [ o ({ x: 29.0, y: 0.0, width: 1.0, height: 1.0 })
  ]

otherOverlappingObjects =
  [ o ({ x: 0.0, y: 30.0, width: 21.0, height: 21.0 })
  , o ({ x: 20.0, y: 40.0, width: 10.0, height: 10.0 })
  ]

print a = trace a \_ -> a

assertLength a l = assert (length (print a) == l)

main :: Effect Unit
main = do
  assert
    -- HorizontalOverlap: horizontally overlapping objects are put into one segment
    ( length
        ( segment horizontal
            horizontallyOverlappingObjects
        )
        == 1
    )
  assert
    -- HorizontalOverlap: non-overlapping objects are left in two separate segments
    ( length
        ( segment horizontal
            nonOverlappingObjects
        )
        == 2
    )
  assert
    -- FullOverlap: horizontally overlapping objects are left in separate segments
    ( length
        ( overlappingSegments
            horizontallyOverlappingObjects
        )
        == 2
    )
  assert
    -- FullOverlap: overlapping objects are in one segment
    ( length
        ( overlappingSegments
            overlappingObjects
        )
        == 1
    )
  assert
    -- FullOverlap: overlapping objects are in one segment
    ( length
        ( overlappingSegments
            (concat [ overlappingObjects, otherOverlappingObjects ])
        )
        == 2
    )
  let
    objects = concat [ overlappingObjects, otherObjects ]
  --FullOverlap: overlapping objects are in one segment
  assertLength
    ( overlappingSegments
        objects
    )
    2
  assertLength
    ( segment horizontal
        objects
    )
    1
  assertLength
    ( segment vertical
        objects
    )
    1
