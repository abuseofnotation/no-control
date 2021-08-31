module Main where

import Data.Maybe
import Data.Traversable
import Graphics.Canvas
import Prelude
import Data.Traversable as Data
import Effect (Effect)
import Effect.Console (log)
import Effect.Console as Effect
import Effect.Ref as Ref
import Prim.Row (class Nub)
import Engine
import Effect.Ref
import Effect.Timer

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
            , width: 100.0
            , height: 100.0
            }
        , params: unit
        }
      , { position:
            { x: 100.0
            , y: 100.0
            , width: 100.0
            , height: 100.0
            }
        , params: unit
        }
      ]
  }

step :: Map -> Map
step gameMap =
  { cameraPosition: gameMap.cameraPosition
  , objects: (map updateObject gameMap.objects)
  }

updateObject :: forall a. GameObject (a) -> GameObject (a)
updateObject object =
  ( { position:
        { x: position.x
        , y: position.y + 1.00
        , width: position.width
        , height: position.height
        }
    , params: object.params
    }
  )
  where
  position = object.position

renderFrame :: Map -> Context2D -> Effect (Map)
renderFrame gameMap ctx = do
  clearRect ctx { x: 0.0, y: 0.0, width: width, height: height }
  Data.traverse_ (\object -> fillPath ctx $ rect ctx object.position)
    gameMap.objects
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
              renderFrame (step oldMap) ctx
          )
          gameMap
      pure unit

width = 800.0

height = 600.0
