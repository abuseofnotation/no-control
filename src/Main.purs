module Main where

import Data.Array
import Data.Maybe
import Data.Traversable
import Effect.Ref
import Effect.Timer
import Graphics.Canvas
import Main.Types
import NoControl.Engine
import Prelude
import Effect (Effect)
import Effect.Class.Console (logShow)
import Effect.Console (log)
import Effect.Console as Effect
import Effect.Ref as Ref
import Main.Level (generateMap)
import Main.Step (step)
import NoControl.Engine.Collisions (overlappingSegments)
import NoControl.Engine.Effect (loop, renderFrame)
import Prim.Row (class Nub)

gameMap :: Map ObjectType
gameMap = generateMap

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
          ( \oldMap keys ->
              renderFrame ctx (step oldMap keys)
          )
          gameMap
      pure unit
