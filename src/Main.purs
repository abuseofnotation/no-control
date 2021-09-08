module Main where

import Data.Array
import Data.Maybe
import Data.Traversable
import Effect.Ref
import Effect.Timer
import Graphics.Canvas
import NoControl.Engine
import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Console as Effect
import Effect.Ref as Ref
import Main.Step (step)
import Main.Types
import NoControl.Engine.Collisions (overlappingSegments)
import NoControl.Engine.Effect (loop, renderFrame)
import Prim.Row (class Nub)

gameMap :: Map ObjectType
gameMap =
  { cameraPosition:
      { width: 800
      , height: 600
      , x: 0
      , y: 0
      }
  , objects:
      [ { position:
            { x: 150.0
            , y: 250.0
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
      , { position:
            { x: 150.0
            , y: 450.0
            , width: 1000.0
            , height: 500.0
            }
        , energy: { x: 0.0, y: 0.0 }
        , characteristics:
            { bounceability:
                0.0
            , maxFallSpeed: 0.0
            }
        , type: Ground
        }
      ]
  }

width = 800.0

height = 600.0

main :: Effect Unit
main = do
  canvas <- getCanvasElementById "app"
  case canvas of
    Nothing -> log "No canvas"
    Just canvas -> do
      setCanvasDimensions canvas { width, height }
      ctx <- getContext2D canvas
      _ <-
        loop
          ( \oldMap ->
              renderFrame ctx (step oldMap)
          )
          gameMap
      pure unit
