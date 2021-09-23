{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "arrays"
  , "assert"
  , "canvas"
  , "console"
  , "debug"
  , "effect"
  , "foldable-traversable"
  , "integers"
  , "js-timers"
  , "lcg"
  , "maybe"
  , "ordered-collections"
  , "prelude"
  , "psci-support"
  , "refs"
  , "tuples"
  , "web-events"
  , "web-html"
  , "web-uievents"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
