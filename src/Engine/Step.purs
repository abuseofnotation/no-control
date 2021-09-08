module NoControl.Engine.Step where

import Prelude
import NoControl.Engine

gravityVector = { x: 0.0, y: 1.0 }

updateObjectEnergy :: forall a. GameObject (a) -> Vector
updateObjectEnergy o =
  if o.characteristics.maxFallSpeed > 0.0 then
    sum o.energy gravityVector
  else
    o.energy

sum :: Vector -> Vector -> Vector
sum v1 v2 = ({ x: (v1.x + v2.x), y: (v1.y + v2.y) })

updateObject :: forall a. GameObject (a) -> GameObject (a)
updateObject object =
  ( { position:
        { x: newPosition.x
        , y: newPosition.y
        , width: position.width
        , height: position.height
        }
    , energy:
        newEnergy
    , characteristics: object.characteristics
    , type: object.type
    }
  )
  where
  position = object.position

  newEnergy = updateObjectEnergy object

  newPosition = sum { x: object.position.x, y: object.position.y } newEnergy
