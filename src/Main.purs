module Main where

import Data.Array
import Data.Maybe
import Data.Traversable
import Effect.Ref
import Effect.Timer
import Graphics.Canvas
import NoControl.Engine
import Prelude
import Data.Array.NonEmpty (length, toArray)
import Data.Traversable as Data
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Console as Effect
import Effect.Ref as Ref
import Prim.Row (class Nub)

gameMap :: Map
gameMap =
  { cameraPosition:
      { width: 800
      , height: 600
      , x: 0
      , y: 0
      }
  , objects:
      [ { position:
            { x: 50.0
            , y: 50.0
            , width: 5.0
            , height: 5.0
            }
        , energy: { x: 3.0, y: -2.0 }
        , characteristics: { maxFallSpeed: 10.0 }
        , params: unit
        }
      , { position:
            { x: 150.0
            , y: 250.0
            , width: 5.0
            , height: 5.0
            }
        , energy: { x: 9.0, y: -20.0 }
        , characteristics: { maxFallSpeed: 10.0 }
        , params: unit
        }
      , { position:
            { x: 150.0
            , y: 250.0
            , width: 5.0
            , height: 5.0
            }
        , energy: { x: 19.0, y: -10.0 }
        , characteristics: { maxFallSpeed: 10.0 }
        , params: unit
        }
      , { position:
            { x: 150.0
            , y: 450.0
            , width: 500.0
            , height: 15.0
            }
        , energy: { x: 0.0, y: 0.0 }
        , characteristics: { maxFallSpeed: 0.0 }
        , params: unit
        }
      ]
  }

step :: Map -> Map
step gameMap =
  { cameraPosition: gameMap.cameraPosition
  , objects: (map updateObjectPosition gameMap.objects)
  }

renderFrame :: Context2D -> Map -> Effect (Map)
renderFrame ctx gameMap = do
  clearRect ctx { x: 0.0, y: 0.0, width: width, height: height }
  Data.traverse_ (\object -> fillPath ctx $ rect ctx object.position)
    gameMap.objects
  --logShow (map (\a -> length a) (overlappingSegments gameMap.objects))
  pure gameMap

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

width = 800.0

height = 600.0
