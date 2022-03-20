{ name = "purescript-doms"
, dependencies =
  [ "arrays"
  , "behaviors"
  , "control"
  , "datetime"
  , "effect"
  , "either"
  , "event"
  , "foldable-traversable"
  , "foreign"
  , "foreign-object"
  , "indexed-monad"
  , "lists"
  , "maybe"
  , "newtype"
  , "nullable"
  , "ordered-collections"
  , "prelude"
  , "record"
  , "refs"
  , "simple-json"
  , "transformers"
  , "tuples"
  , "typelevel"
  , "typelevel-peano"
  , "unsafe-coerce"
  , "variant"
  , "web-dom"
  , "web-events"
  ]
, packages = ./packages.dhall
, sources = [ "src/**/*.purs" ]
}