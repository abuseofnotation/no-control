module NoControl.Engine.Collisions where

import Data.Array
import Data.Maybe
import Prelude
import Data.Traversable as Data
import NoControl.Engine

type BoundaryAccessor a
  = { beginning ::
        GameObject a ->
        Number
    , ending ::
        GameObject a -> Number
    }

horizontal :: forall a. BoundaryAccessor a
horizontal =
  { beginning: \o -> o.position.x
  , ending: \o -> o.position.x + o.position.width
  }

vertical :: forall a. BoundaryAccessor a
vertical =
  { beginning: \o -> o.position.y
  , ending: \o -> o.position.y + o.position.height
  }

--segmentHelper :: forall a. BoundaryAccessor a -> Array (Objects a) -> GameObject a
segmentHelper f segments object = case last segments >>= last of
  Nothing -> [ [ object ] ]
  Just lastObject ->
    if (f.ending lastObject > f.beginning object) then
      -- the last object in the segment should always be the one which is at its
      -- boundary (which is not always the one which is closest to it, due to 
      -- some objects being wider than others)
      if f.ending lastObject > f.ending object then
        fromMaybe [] (modifyAt lastSegmentIndex (\s -> cons object s) segments)
      else
        fromMaybe [] (modifyAt lastSegmentIndex (\s -> snoc s object) segments)
    else
      snoc segments [ object ]
  where
  lastSegmentIndex = length segments - 1

segment f o = foldl (segmentHelper f) [] sorted
  where
  sorted = sortWith f.beginning o

overlappingSegments :: forall a. Objects a -> Array (Objects a)
overlappingSegments o = (segment horizontal) o >>= (segment vertical)

type HandleCollision a
  = Objects a -> Objects a

handleObjectCollisions :: forall a. HandleCollision a -> Objects a -> Objects a
handleObjectCollisions fn objects = overlappingSegments objects >>= fn
