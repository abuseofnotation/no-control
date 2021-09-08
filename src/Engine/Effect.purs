module NoControl.Engine.Effect where

import Effect.Timer
import NoControl.Engine
import Prelude
import Data.Array (length)
import Data.Traversable as Data
import Effect (Effect)
import Effect as NoControl.Engine
import Effect.Console (logShow)
import Effect.Ref as Ref
import Graphics.Canvas (Context2D, clearRect, fillPath, rect)

width = 800.0

height = 600.0

loop :: forall a. (a -> Effect a) -> a -> Effect (IntervalId)
loop fn beginningMap = do
  mapState <- Ref.new beginningMap
  setInterval 50 do
    oldMap <- Ref.read mapState
    newMap <- fn oldMap
    Ref.write newMap mapState
    pure unit

renderFrame :: forall a. Context2D -> Map a -> Effect (Map a)
renderFrame ctx gameMap = do
  clearRect ctx { x: 0.0, y: 0.0, width: width, height: height }
  Data.traverse_ (\object -> fillPath ctx $ rect ctx object.position)
    gameMap.objects
  pure gameMap
