module Main where

import Control.Monad.Loops
import Data.Maybe
import Data.Traversable
import Effect.Ref
import Effect.Timer
import Graphics.Canvas
import Prelude
import Effect (Effect)
import Effect.Console (log)
import Effect.Console as Effect
import Effect.Ref as Ref
import Prim.Row (class Nub)

type Coordinates
  = { x :: Int, y :: Int }

type ObjectPosition
  = { width :: Number
    , height :: Number
    , x :: Number
    , y :: Number
    }

type GameObject a
  = { position :: ObjectPosition
    , params :: a
    }

type Objects
  = Array (GameObject (Unit))

type Map
  = { cameraPosition ::
        { width :: Int
        , height :: Int
        , x :: Int
        , y :: Int
        }
    , objects :: Objects
    }

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

renderFrame :: Map -> Context2D -> Effect (Unit)
renderFrame gameMap ctx =
  traverse
    ( \object -> fillPath ctx $ rect ctx object.position
    )
    gameMap.objects
    >>= (\a -> Effect.info "foo")

main :: Effect Unit
main = do getCanvasElementById "app" >>= processCanvas

width = 800.0

height = 600.0

processCanvas :: Maybe CanvasElement -> Effect Unit
processCanvas (Just canvas) = init (canvas)

processCanvas Nothing = log "No canvas"

step :: Map -> Context2D -> Effect Map
step gameMap ctx = do
  pure
    { cameraPosition: gameMap.cameraPosition
    , objects: (map updateObject gameMap.objects)
    }

updateObject :: forall a. GameObject (a) -> GameObject (a)
updateObject object =
  ( { position:
        { x: position.x - 1.0
        , y: position.y
        , width: position.width
        , height: position.height
        }
    , params: object.params
    }
  )
  where
  position = object.position

init :: CanvasElement -> Effect Unit
init canvas = do
  setCanvasDimensions canvas { width, height }
  ctx <- getContext2D canvas
  mapState <- Ref.new gameMap
  _ <-
    setInterval 100 do
      oldMap <- Ref.read mapState
      newMap <- step oldMap ctx
      Ref.write newMap mapState
      renderFrame newMap ctx
      pure unit
  pure unit
