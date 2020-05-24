{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "password-generator"
, dependencies =
  [ "concur-react"
  , "halogen"
  , "test-unit"
  , "psci-support"
  ]
, packages = ./packages.dhall
}
