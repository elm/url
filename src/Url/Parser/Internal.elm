module Url.Parser.Internal exposing
  ( QueryParser(..)
  , Parameters
  )


type QueryParser a =
  Parser (Parameters -> a)


type alias Parameters =
  Dict.Dict String (List String)
