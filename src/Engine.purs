module Engine where

import Prelude
import Effect (Effect)
import Effect.Ref as Ref
import Effect.Timer

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

loop :: (Map -> Effect (Map)) -> Map -> Effect (IntervalId)
loop fn beginningMap = do
  mapState <- Ref.new beginningMap
  setInterval 50 do
    oldMap <- Ref.read mapState
    newMap <- fn oldMap
    Ref.write newMap mapState
    pure unit
