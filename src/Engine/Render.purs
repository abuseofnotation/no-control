module NoControl.Engine.Render where

import Data.Array
import Data.Maybe
import Prelude
import Data.Traversable as Data
import NoControl.Engine (GameObject, Map, Objects, ObjectPosition)

updateCamera :: forall a b. Map b -> Objects a -> Objects a
updateCamera gameMap = map (updateCoordinates gameMap.cameraPosition)

updateCoordinates :: forall a. ObjectPosition -> GameObject a -> GameObject a
updateCoordinates camera a =
  { position:
      { x: a.position.x - (camera.x - 400.0) * a.characteristics.distance
      , y: a.position.y - (camera.y - 300.0) * a.characteristics.distance
      , width:
          a.position.width
      , height:
          a.position.height
      }
  , characteristics: a.characteristics
  , type:
      a.type
  , energy:
      { x:
          a.energy.x
      , y:
          a.energy.y
      }
  }

cameraFollowObject :: forall a. Maybe (GameObject a) -> ObjectPosition -> ObjectPosition
cameraFollowObject (Just player) position =
  { x:
      player.position.x
  , y:
      player.position.y
  , width:
      position.width
  , height: position.height
  }

cameraFollowObject Nothing position = position
