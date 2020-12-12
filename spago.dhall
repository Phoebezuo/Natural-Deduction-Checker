{-
Welcome to a Spago project!
You can edit this file as you like.
-}
{ name = "my-project"
, dependencies =
  [ "aff-promise"
  , "argonaut-core"
  , "console"
  , "control"
  , "effect"
  , "foldable-traversable"
  , "foreign-object"
  , "functors"
  , "halogen"
  , "node-buffer"
  , "node-fs"
  , "ordered-collections"
  , "parsing"
  , "psci-support"
  , "strings"
  , "stringutils"
  , "test-unit"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs", "test/**/*.purs" ]
}
