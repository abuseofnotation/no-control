module Main.Step where

import Data.Array
import Main.Types
import Prelude
import Prim.Boolean
import Data.Array (findIndex, modifyAt)
import Data.Int (toNumber)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Set as Set
import Debug (spy, trace)
import Main.Level (groundZero, playerInitialState, towerDistance, towerFloorHeight, towers)
import NoControl.Engine (Map, ObjectPosition, Objects, GameObject)
import NoControl.Engine.Collisions (handleObjectCollisions)
import NoControl.Engine.Render (cameraFollowObject)
import NoControl.Engine.Step (updateObject)
import Web.HTML.HTMLCanvasElement (height, width)
import Web.HTML.HTMLProgressElement (position)

jumpPower = 15.0

walkingPower = 5.0

jumpControlPower = 0.2

generateBombs =
  map
    ( \i ->
        { position:
            { x: (toNumber i) * towerDistance - 30.00
            , y: 0.0
            , width: 10.0
            , height: 10.0
            }
        , energy: { x: -2.0, y: 0.0 }
        , characteristics:
            { bounceability:
                0.6
            , maxFallSpeed:
                100.0
            , color: "blue"
            , distance: 1.0
            }
        , type: Bomb
        }
    )
    (range 1 towers)

step :: Map ObjectType -> Set.Set String -> Map ObjectType
step gameMap keys =
  { cameraPosition: cameraFollowPlayer player gameMap.cameraPosition
  , objects: updateObjects player keys gameMap.objects
  , foreground: gameMap.foreground
  , background: gameMap.background
  }
  where
  player = find isPlayer gameMap.objects

updateObjects player keys =
  map updateObject
    >>> handleObjectCollisions handleCollisions
    >>> moveObjects keys
    >>> filter (\o -> o.position.y < 10000.0)
    >>> respawnPlayerIfNeeded
    >>> dropBombsIfNeeded

respawnPlayerIfNeeded :: Objects ObjectType -> Objects ObjectType
respawnPlayerIfNeeded o = case p of
  Just player -> o
  Nothing -> concat [ [ playerInitialState ], o ]
  where
  p = find isPlayer o

dropBombsIfNeeded :: Objects ObjectType -> Objects ObjectType
dropBombsIfNeeded o =
  if length currentBombs < bombs then
    trace bombs \_ -> concat [ o, (generateBombs) ]
  else
    o
  where
  currentBombs = filter isBomb o

  bombs = towers * 10

cameraFollowPlayer player =
  cameraFollowObject player
    >>> ( \camera ->
          if camera.y + camera.height > groundZero then
            { x: camera.x
            , y: groundZero - camera.height
            , width: camera.width
            , height: camera.height
            }
          else
            camera
      )

moveObjects :: Set.Set String -> Objects ObjectType -> Objects ObjectType
moveObjects keys o = case findIndex isPlayer o of
  Just playerIndex -> fromMaybe o (modifyAt playerIndex (movePlayer keys) o)
  Nothing -> o

movePlayer :: Set.Set String -> GameObject ObjectType -> GameObject ObjectType
movePlayer keys a =
  { position: a.position
  , characteristics: a.characteristics
  , type:
      a.type
  , energy:
      { x:
          if Set.member "ArrowRight" keys then
            a.energy.x + speed
          else if Set.member "ArrowLeft" keys then
            a.energy.x - speed
          else
            a.energy.x
      , y:
          if (Set.member "ArrowUp" keys && onGround) then
            a.energy.y - jumpPower
          else
            a.energy.y
      }
  }
  where
  onGround = a.energy.y == 0.0

  speed = if onGround then walkingPower else jumpControlPower

isPlayer o = case o.type of
  Player -> true
  _ -> false

isBomb o = case o.type of
  Bomb -> true
  _ -> false

bounce :: forall a. GameObject a -> GameObject a -> GameObject a
bounce ground a =
  { position:
      { x: a.position.x
      -- Simplistic algorithm, supports only horizonal and vertical clashes
      , y:
          if a.energy.y > 0.0 then
            ground.position.y - a.position.height
          else
            ground.position.y + ground.position.height
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

fall :: GameObject ObjectType -> GameObject ObjectType -> GameObject ObjectType
fall bomb a = case a.type of
  Player ->
    { position: a.position
    , characteristics: a.characteristics
    , type:
        a.type
    , energy:
        { x: 0.0
        , y: 20.0
        }
    }
  _ -> (bounce bomb a)

sortByType :: Objects ObjectType -> Objects ObjectType
sortByType =
  sortWith
    ( \o -> case o.type of
        Ground -> 1
        Bomb -> 2
        Player -> 3
        None -> 4
    )

groundCollide :: GameObject ObjectType -> GameObject ObjectType -> Array (GameObject ObjectType)
groundCollide ground object = [ ground, object ]

handleCollisions :: Objects ObjectType -> Objects ObjectType
handleCollisions = sortByType >>> \array -> handleCollision (head array) (fromMaybe [] (tail array))

handleCollision :: Maybe (GameObject ObjectType) -> Objects ObjectType -> Objects ObjectType
handleCollision (Just x@{ type: Ground }) xs = x : (map (bounce x) xs)

handleCollision (Just x@{ type: Player }) xs = x : (map (bounce x) xs)

handleCollision (Just x@{ type: Bomb }) xs = x : (map (fall x) xs)

handleCollision (Just x@{ type: None }) xs = x : (map (bounce x) xs)

handleCollision Nothing _ = []
