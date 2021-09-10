module Main.Level where

import Prelude
import Data.Array (cons, range)
import Data.Int (toNumber)
import Main.Types (ObjectType(..))
import NoControl.Engine (ObjectPosition, Objects, GameObject)
import Web.HTML.HTMLCanvasElement (height)

towerDistance = 900.0

towerFloors = 20

towerFloorHeight = 100.0

towerFloorWidth = 400.0

towers = 10

type FloorGenerator a
  = ObjectPosition -> Objects a

floorGround :: FloorGenerator (ObjectType)
floorGround position =
  [ emptyObject Ground
      { x:
          position.x
      , y:
          position.y
      , width:
          position.width
      , height:
          1.0
      }
  ]

decor :: FloorGenerator Unit
decor position =
  -- ceiling
  [ emptyObject unit
      { x:
          position.x
      , y:
          position.y
      , width:
          position.width
      , height:
          10.0
      }
  --floor
  , emptyObject unit
      { x:
          position.x
      , y:
          position.y + position.height - 30.0
      , width:
          position.width
      , height:
          30.0
      }
  --left end
  , emptyObject unit
      { x:
          position.x
      , y:
          position.y
      , width:
          50.0
      , height:
          position.height
      }
  -- right end
  , emptyObject unit
      { x:
          position.x + position.width - 50.0
      , y:
          position.y
      , width:
          50.0
      , height:
          position.height
      }
  -- middle
  , emptyObject unit
      { x:
          position.x + (position.width - 70.0) / 2.0
      , y:
          position.y
      , width:
          70.0
      , height:
          position.height
      }
  ]

generateTowers :: forall a. FloorGenerator a -> Objects a
generateTowers fn = bind (range 0 towers) (generateTower fn)

generateTower :: forall a. FloorGenerator a -> Int -> Objects a
generateTower fn towerNumber = bind (range 0 towerFloors) (generateFloor fn towerNumber)

emptyObject :: forall a. a -> ObjectPosition -> GameObject a
emptyObject objectType position =
  { position: position
  , energy: { x: 0.0, y: 0.0 }
  , characteristics:
      { bounceability:
          0.0
      , maxFallSpeed: 0.0
      }
  , type: objectType
  }

generateFloor :: forall a. FloorGenerator a -> Int -> Int -> Objects a
generateFloor fn towerNumber floorNumber =
  fn
    { x: x
    , y: y
    , width: towerFloorWidth
    , height: towerFloorHeight
    }
  where
  x = toNumber towerNumber * towerDistance

  y = toNumber floorNumber * towerFloorHeight

generateObjects :: Objects ObjectType
generateObjects =
  cons
    { position:
        { x: towerFloorWidth / 2.0
        , y: 0.0
        , width: 25.0
        , height: 50.0
        }
    , energy: { x: 1.0, y: -10.0 }
    , characteristics:
        { bounceability:
            0.0
        , maxFallSpeed:
            10.0
        }
    , type: Player
    }
    ( generateTowers
        floorGround
    )

generateMap =
  { cameraPosition:
      { width: 800
      , height: 600
      , x: 0
      , y: 0
      }
  , objects: generateObjects
  , foreground: (generateTowers decor)
  , background: []
  }
