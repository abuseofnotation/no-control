module NoControl.Engine where

import Prelude
import Data.Traversable as Data
import Data.Array
import Data.Maybe

type ObjectPosition
  = { width :: Number
    , height :: Number
    , x :: Number
    , y :: Number
    }

type Vector
  = { x :: Number
    , y :: Number
    }

type ObjectCharacteristics
  = { maxFallSpeed ::
        Number
    , bounceability ::
        Number
    , color :: String
    , distance :: Number
    }

type GameObject a
  = { position :: ObjectPosition
    , energy :: Vector
    , characteristics :: ObjectCharacteristics
    , type :: a
    }

type Objects a
  = Array (GameObject (a))

type Map a
  = { cameraPosition :: ObjectPosition
    , objects :: Objects a
    , foreground :: Objects Unit
    , background :: Objects Unit
    }
