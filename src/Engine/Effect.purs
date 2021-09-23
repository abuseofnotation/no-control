module NoControl.Engine.Effect where

import Prelude
import Data.Set as Set
import Data.Traversable (traverse_)
import Data.Traversable as Data
import Effect (Effect)
import Effect.Ref (Ref)
import Effect.Ref as Ref
import Effect.Timer (IntervalId, setInterval)
import Graphics.Canvas (Context2D, clearRect, fillPath, rect, setFillStyle)
import NoControl.Engine (Objects, Map)
import NoControl.Engine.Render (updateCamera)
import Web.Event.EventTarget (addEventListener, eventListener)
import Web.HTML (window)
import Web.HTML.Window (toEventTarget)
import Web.UIEvent.KeyboardEvent (code, fromEvent)
import Web.UIEvent.KeyboardEvent.EventTypes (keydown, keyup)

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
  setInterval 20 do
    pressedKeys <- Ref.read keys
    oldMap <- Ref.read mapState
    newMap <- fn oldMap pressedKeys
    Ref.write newMap mapState
    pure unit

renderLayer :: forall a. Context2D -> Objects a -> Effect Unit
renderLayer ctx =
  Data.traverse_ \object -> do
    setFillStyle ctx object.characteristics.color
    fillPath ctx $ rect ctx object.position

renderFrame :: forall a. Context2D -> Map a -> Effect (Map a)
renderFrame ctx gameMap = do
  clearRect ctx { x: 0.0, y: 0.0, width: gameMap.cameraPosition.width, height: gameMap.cameraPosition.height }
  renderLayer ctx (updateCamera gameMap gameMap.background)
  renderLayer ctx (updateCamera gameMap gameMap.objects)
  renderLayer ctx (updateCamera gameMap gameMap.foreground)
  pure gameMap
