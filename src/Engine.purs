module NoControl.Engine where

import Prelude
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Timer
import Data.Traversable as Data
import Data.Array
import Data.Maybe

type ObjectPosition
  = { width :: Number
    , height :: Number
    , x :: Number
    , y :: Number
    }

type Vector
  = { x :: Number
    , y :: Number
    }

gravityVector = { x: 0.0, y: 1.0 }

updateObjectEnergy :: forall a. GameObject (a) -> Vector
updateObjectEnergy o =
  if o.characteristics.maxFallSpeed > 0.0 then
    sum o.energy gravityVector
  else
    o.energy

sum :: Vector -> Vector -> Vector
sum v1 v2 = ({ x: (v1.x + v2.x), y: (v1.y + v2.y) })

type ObjectCharacteristics
  = { maxFallSpeed :: Number
    }

type GameObject a
  = { position :: ObjectPosition
    , energy :: Vector
    , characteristics :: ObjectCharacteristics
    , params :: a
    }

type Objects
  = Array (GameObject (Unit))

type Map
  = { cameraPosition ::
        { width :: Int
        , height :: Int
        , x :: Int
        , y :: Int
        }
    , objects :: Objects
    }

updateObjectPosition :: forall a. GameObject (a) -> GameObject (a)
updateObjectPosition object =
  ( { position:
        { x: newPosition.x
        , y: newPosition.y
        , width: position.width
        , height: position.height
        }
    , energy:
        newEnergy
    , characteristics: object.characteristics
    , params: object.params
    }
  )
  where
  position = object.position

  newEnergy = updateObjectEnergy object

  newPosition = sum { x: object.position.x, y: object.position.y } newEnergy

loop :: (Map -> Effect (Map)) -> Map -> Effect (IntervalId)
loop fn beginningMap = do
  mapState <- Ref.new beginningMap
  setInterval 50 do
    oldMap <- Ref.read mapState
    newMap <- fn oldMap
    Ref.write newMap mapState
    pure unit

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
verticalOverlap left right = (left.position.y + left.position.height) > right.position.y

overlappingSegments :: Objects -> Array (Objects)
overlappingSegments o = segmentBy horizontalOverlap o -- >>= segmentBy verticalOverlap

type HandleCollision
  = Objects -> Objects

handleObjectCollisions :: HandleCollision -> Map -> Map
handleObjectCollisions fn map =
  { cameraPosition: map.cameraPosition
  , objects: overlappingSegments map.objects >>= fn
  }
