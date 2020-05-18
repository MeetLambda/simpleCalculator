{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "password-generator"
, dependencies =
  [ "console"
  , "effect"
  , "halogen"
  , "profunctor-lenses"
  , "psci-support"
  , "test-unit"
  ]
, packages = ./packages.dhall
}
