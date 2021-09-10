module NoControl.Engine.Effect where

import Effect.Timer
import NoControl.Engine
import Prelude
import Data.Array (length)
import Data.Set as Set
import Data.Traversable (traverse_)
import Data.Traversable as Data
import Debug (spy)
import Effect (Effect)
import Effect as NoControl.Engine
import Effect.Console (logShow)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Graphics.Canvas (Context2D, clearRect, fillPath, rect)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)
import Web.UIEvent.KeyboardEvent (code, fromEvent)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown, keyup)

width = 800.0

height = 600.0

keyReader :: Effect (Ref (Set.Set (String)))
keyReader = do
  keys <- Ref.new Set.empty
  target <- toEventTarget <$> window
  keyDownListener <-
    eventListener \e -> do
      fromEvent e
        # traverse_ \ke ->
            Ref.modify (Set.insert (code ke)) keys
  keyUpListener <-
    eventListener \e -> do
      fromEvent e
        # traverse_ \ke ->
            Ref.modify (Set.delete (code ke)) keys
  addEventListener keydown keyDownListener false target
  addEventListener keyup keyUpListener false target
  pure keys

loop :: forall a. (a -> Set.Set String -> Effect a) -> a -> Effect (IntervalId)
loop fn beginningMap = do
  keys <- keyReader
  mapState <- Ref.new beginningMap
  setInterval 50 do
    keys <- Ref.read keys
    oldMap <- Ref.read mapState
    newMap <- fn oldMap keys
    Ref.write newMap mapState
    pure unit

renderFrame :: forall a. Context2D -> Map a -> Effect (Map a)
renderFrame ctx gameMap = do
  clearRect ctx { x: 0.0, y: 0.0, width: width, height: height }
  Data.traverse_ (\object -> fillPath ctx $ rect ctx object.position)
    gameMap.objects
  Data.traverse_ (\object -> fillPath ctx $ rect ctx object.position)
    gameMap.foreground
  pure gameMap
