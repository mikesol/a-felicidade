{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "audio-behaviors"
  , "console"
  , "effect"
  , "psci-support"
  , "typelevel-graph"
  , "typelevel-klank-dev"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
