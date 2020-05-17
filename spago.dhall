{ sources = [ "src/**/*.purs", "test/**/*.purs" ]
, name = "password-generator"
, dependencies = [ "console", "effect", "halogen", "profunctor-lenses" ]
, packages = ./packages.dhall
}
