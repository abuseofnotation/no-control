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

towers = 10

type FloorGenerator a
  = ObjectPosition -> Objects a

ground =
  { position:
      { y: towerFloorHeight * (toNumber (towerFloors + 3))
      , x: 0.0
      , width: 1000.0
      , height: 190.0
      }
  , energy: { x: 0.0, y: 0.0 }
  , characteristics:
      { bounceability:
          0.0
      , maxFallSpeed:
          0.0
      , color: "red"
      , distance: 1.0
      }
  , type: Ground
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
          1.0
      }
      "black"
  ]

backgroundDecor =
  [ { position:
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
  , { position:
        { x: -100.0
        , y: 100.0
        , width: 200.0
        , height: 1600.0
        }
    , energy: { x: 0.0, y: 0.0 }
    , characteristics:
        { bounceability:
            0.0
        , maxFallSpeed: 0.0
        , color: "darkslategrey"
        , distance: 0.1
        }
    , type: unit
    }
  , { position:
        { x: 600.0
        , y: 100.0
        , width: 200.0
        , height: 1600.0
        }
    , energy: { x: 0.0, y: 0.0 }
    , characteristics:
        { bounceability:
            0.0
        , maxFallSpeed: 0.0
        , color: "darkslategrey"
        , distance: 0.1
        }
    , type: unit
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
      "slategrey"
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
      "slategrey"
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
      "slategrey"
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
      "slategrey"
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
      "slategrey"
  ]

background :: FloorGenerator Unit
background position =
  [ emptyObject unit position "palegoldenrod"
  ]

generateTowers :: forall a. FloorGenerator a -> Objects a
generateTowers fn = bind (range 0 towers) (generateTower fn)

generateTower :: forall a. FloorGenerator a -> Int -> Objects a
generateTower fn towerNumber = bind (range 0 towerFloors) (generateFloor fn towerNumber)

emptyObject :: forall a. a -> ObjectPosition -> String -> GameObject a
emptyObject objectType position color =
  { position: position
  , energy: { x: 0.0, y: 0.0 }
  , characteristics:
      { bounceability:
          0.0
      , maxFallSpeed: 0.0
      , color: color
      , distance: 1.0
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
    [ [ { position:
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
      -- ground
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
