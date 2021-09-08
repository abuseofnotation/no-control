module Main.Step where

import Data.Array
import Main.Types
import Prelude
import Data.Maybe (Maybe(..), fromMaybe)
import Debug (spy, trace)
import NoControl.Engine (Map, Objects, GameObject)
import NoControl.Engine.Collisions (handleObjectCollisions)
import NoControl.Engine.Step (updateObject)
import Prim.Boolean

step :: Map ObjectType -> Map ObjectType
step gameMap =
  { cameraPosition: gameMap.cameraPosition
  , objects: updateObjects gameMap.objects
  }

updateObjects = updateCamera <<< handleObjectCollisions handleCollisions <<< map updateObject

updateCamera objects = map (updateCoordinates player) objects
  where
  player = find isPlayer objects

isPlayer o = case o.type of
  Player -> true
  _ -> false

updateCoordinates :: Maybe (GameObject ObjectType) -> GameObject ObjectType -> GameObject ObjectType
updateCoordinates (Just player) a =
  { position:
      { x: a.position.x - player.position.x + 400.0
      , y: a.position.y - player.position.y + 300.0
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

updateCoordinates Nothing a = a

bounce :: forall a. GameObject a -> GameObject a -> GameObject a
bounce ground a =
  { position:
      { x: a.position.x
      , y: ground.position.y - a.position.height
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
          a.energy.x * a.characteristics.bounceability
      , y:
          -(a.energy.y * a.characteristics.bounceability)
      }
  }

sortByType :: Objects ObjectType -> Objects ObjectType
sortByType =
  sortWith
    ( \o -> case o.type of
        Ground -> 1
        Player -> 2
        Bomb -> 3
        None -> 4
    )

groundCollide :: GameObject ObjectType -> GameObject ObjectType -> Array (GameObject ObjectType)
groundCollide ground object = [ ground, object ]

handleCollisions :: Objects ObjectType -> Objects ObjectType
handleCollisions = sortByType >>> \array -> handleCollision (head array) (fromMaybe [] (tail array))

handleCollision :: Maybe (GameObject ObjectType) -> Objects ObjectType -> Objects ObjectType
handleCollision (Just x@{ type: Ground }) xs = x : (map (bounce x) xs)

handleCollision (Just x@{ type: Player }) xs = x : (map (bounce x) xs)

handleCollision (Just x@{ type: Bomb }) xs = x : (map (bounce x) xs)

handleCollision (Just x@{ type: None }) xs = x : (map (bounce x) xs)

handleCollision Nothing _ = []
