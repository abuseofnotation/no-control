module Main.Level where

import Prelude
import Data.Array (concat, cons, range)
import Data.Int (toNumber)
import Main.Types (ObjectType(..))
import NoControl.Engine (ObjectPosition, Objects, GameObject)
import Web.HTML.HTMLCanvasElement (height)
import Web.HTML.HTMLProgressElement (position)

towerDistance = 900.0

towerFloors = 20

towerFloorHeight = 100.0

towerFloorWidth = 400.0

towers = 20

groundZero = towerFloorHeight * (toNumber (towerFloors + 2))

type FloorGenerator a
  = ObjectPosition -> Objects a

playerInitialState =
  { position:
      { x: towerFloorWidth / 2.0
      , y: 0.0
      , width: 14.0
      , height: 50.0
      }
  , energy: { x: 1.0, y: -10.0 }
  , characteristics:
      { bounceability:
          0.0
      , maxFallSpeed:
          10.0
      , color: "red"
      , distance: 1.0
      }
  , type: Player
  }

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
          10.0
      }
      "black"
      1.0
  ]

blackBackground =
  { position:
      { x: 0.0
      , y: 0.0
      , width: 2000.0
      , height: 1600.0
      }
  , energy: { x: 0.0, y: 0.0 }
  , characteristics:
      { bounceability:
          0.0
      , maxFallSpeed: 0.0
      , color: "black"
      , distance: 0.000001
      }
  , type: unit
  }

backgroundObject position = emptyObject unit position "darkslategrey" 0.3

backgroundDecor =
  [ blackBackground
  , backgroundObject
      { x: -100.0
      , y: 100.0
      , width: 200.0
      , height: 1600.0
      }
  , backgroundObject
      { x: 600.0
      , y: 100.0
      , width: 200.0
      , height: 1600.0
      }
  , backgroundObject
      { x: 900.0
      , y: 300.0
      , width: 200.0
      , height: 1600.0
      }
  , backgroundObject
      { x: 600.0
      , y: 600.0
      , width: 200.0
      , height: 1600.0
      }
  , backgroundObject
      { x: 1600.0
      , y: 600.0
      , width: 200.0
      , height: 1600.0
      }
  , backgroundObject
      { x: 2600.0
      , y: 400.0
      , width: 200.0
      , height: 1600.0
      }
  ]

decorObject position = emptyObject unit position "slategrey" 1.0

decor :: FloorGenerator Unit
decor position =
  -- ceiling
  [ decorObject
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
  , decorObject
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
  , decorObject
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
  , decorObject
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
  , decorObject
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

background :: FloorGenerator Unit
background position =
  [ emptyObject unit position "palegoldenrod" 1.0
  ]

generateTowers :: forall a. FloorGenerator a -> Objects a
generateTowers fn = bind (range 0 towers) (generateTower fn)

generateTower :: forall a. FloorGenerator a -> Int -> Objects a
generateTower fn towerNumber = bind (range 0 towerFloors) (generateFloor fn towerNumber)

emptyObject :: forall a. a -> ObjectPosition -> String -> Number -> GameObject a
emptyObject objectType position color distance =
  { position: position
  , energy: { x: 0.0, y: 0.0 }
  , characteristics:
      { bounceability:
          0.0
      , maxFallSpeed: 0.0
      , color: color
      , distance: distance
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
  concat
    [ [ playerInitialState
      ]
    , ( generateTowers
          floorGround
      )
    ]

generateMap =
  { cameraPosition:
      { width: 1000.0
      , height: 800.0
      , x: 0.0
      , y: 0.0
      }
  , objects: generateObjects
  , foreground: concat [ (generateTowers decor) ]
  , background: concat [ backgroundDecor, (generateTowers background) ]
  }
