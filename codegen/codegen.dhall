{ name = "kievit-fragments"
, dependencies =
    [ "debug"
    , "effect"
    , "aff"
    , "tuples"
    , "foreign-object"
    , "tuples"
    , "fetch"
    , "argonaut-codecs"
    , "console"
    , "tidy-codegen"
    , "node-buffer"
    , "node-fs-aff"
    , "language-cst-parser"
    , "argonaut-core"
    , "either"
    , "foldable-traversable"
    , "maybe"
    , "partial"
    , "prelude"
    , "transformers"
    , "ordered-collections"
    , "arrays"
    , "strings"
    ]
, sources =
    [ "codegen/**/*.purs"
    ]
, packages = ./../packages.dhall
}