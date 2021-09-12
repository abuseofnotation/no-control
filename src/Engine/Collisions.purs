module NoControl.Engine.Collisions where

import Data.Array
import Data.Maybe
import Prelude
import Data.Traversable as Data
import NoControl.Engine

segmentByHelper :: forall a. (a -> a -> Boolean) -> Array (Array (a)) -> a -> Array (Array (a))
segmentByHelper fn segments object = case last segments >>= last of
  Nothing -> [ [ object ] ]
  Just lastObject ->
    if (fn lastObject object) then
      fromMaybe [] (modifyAt lastSegmentIndex (\s -> snoc s object) segments)
    else
      snoc segments [ object ]
  where
  lastSegmentIndex = length segments - 1

segmentBy :: forall a. (a -> a -> Boolean) -> Array (a) -> Array (Array (a))
segmentBy fn = foldl (segmentByHelper fn) []

horizontalOverlap :: forall a. GameObject (a) -> GameObject (a) -> Boolean
horizontalOverlap left right = (left.position.x + left.position.width) > right.position.x

verticalOverlap :: forall a. GameObject (a) -> GameObject (a) -> Boolean
verticalOverlap down up = (down.position.y + down.position.height) > up.position.y

sortByX = sortWith \obj -> obj.position.x

sortByY = sortWith \obj -> obj.position.y

overlappingSegments :: forall a. Objects a -> Array (Objects a)
overlappingSegments o =
  segmentBy horizontalOverlap (sortByX o) >>= (\o -> segmentBy verticalOverlap (sortByY o))
    -- Have to run the horizontal overlap the second time, as several objects (more than two)
    
    -- may be in one segment without actually overlapping
    
    >>= (\o -> segmentBy horizontalOverlap (sortByX o))

type HandleCollision a
  = Objects a -> Objects a

handleObjectCollisions :: forall a. HandleCollision a -> Objects a -> Objects a
handleObjectCollisions fn objects = overlappingSegments objects >>= fn
