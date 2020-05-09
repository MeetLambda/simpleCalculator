{
    sources = [ "src/**/*.purs", "test/**/*.purs" ],
    name = "password-generator",
    dependencies =  [ "console", "effect", "halogen" ],
    packages = ./packages.dhall
}
