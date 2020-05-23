{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "password-generator"
, dependencies =
  [ "concur-react"
  , "console"
  , "effect"
  , "halogen"
  , "profunctor-lenses"
  , "psci-support"
  , "test-unit"
  ]
, packages = ./packages.dhall
}
