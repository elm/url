module Url.Parser.Query exposing
  ( Parser, string, int, enum, custom
  , parse, parseWithLeftovers
  , map, map2, map3, map4, map5, map6, map7, map8
  )

{-| In [the URI spec](https://tools.ietf.org/html/rfc3986), Tim Breners-Lee
says a URL looks like this:

```
  https://example.com:8042/over/there?name=ferret#nose
  \___/   \______________/\_________/ \_________/ \__/
    |            |            |            |        |
  scheme     authority       path        query   fragment
```

This module is for parsing the `query` part.

In this library, a valid query looks like `?search=hats&page=2` where each
query parameter has the format `key=value` and is separated from the next
parameter by the `&` character.

# Parse Query Parameters
@docs Parser, string, int, enum, custom

# Run a Parser
@docs parse, parseWithLeftovers

# Mapping
@docs map, map2, map3, map4, map5, map6, map7, map8

-}

import Dict
import Url.Builder exposing (percentDecode)



-- PARSERS


{-| Parse a query like `?search=hat&page=2` into nice Elm data.
-}
type Parser a =
  Parser (Parameters -> (a, Parameters))


type alias Parameters =
  Dict.Dict String (Status, List String)


type Status = Visited | Unvisited


type Problem = NotFound | Invalid String | TooMany (List String)



-- PRIMITIVES


{-| Handle `String` parameters.

    search : Parser (Result Problem String)
    search =
      string "search"

    -- ?search=cats             == Ok "cats"
    -- ?search=42               == Ok "42"
    -- ?branch=left             == Err NotFound
    -- ?search=cats&search=dogs == Err (TooMany ["cats","dogs"])

Check out [`custom`](#custom) if you need to handle multiple `search`
parameters for some reason.
-}
string : String -> Parser (Result Problem String)
string key =
  custom key <| \stringList ->
    case stringList of
      [] ->
        Err NotFound

      [string] ->
        Ok string

      _ ->
        Err (TooMany stringList)


{-| Handle `Int` parameters. Maybe you want to show paginated search results:

    page : Parser (Result Problem Int)
    page =
      int "page"

    -- ?page=2        == Ok 2
    -- ?page=17       == Ok 17
    -- ?page=two      == Err (Invalid "two")
    -- ?sort=date     == Err NotFound
    -- ?page=2&page=3 == Err (TooMany ["2","3"])

Check out [`custom`](#custom) if you need to handle multiple `page` parameters
or something like that.
-}
int : String -> Parser (Result Problem Int)
int key =
  custom key <| \stringList ->
    case stringList of
      [] ->
        Err NotFound

      [string] ->
        case String.toInt string of
          Nothing ->
            Err (Invalid string)

          Just n ->
            Ok n

      _ ->
        Err (TooMany stringList)


{-| Handle enumerated parameters. Maybe you want a true-or-false parameter:

    import Dict

    debug : Parser (Result Problem Bool)
    debug =
      enum "debug" (Dict.fromList [ ("true", True), ("false", False) ])

    -- ?debug=true   == Ok True
    -- ?debug=false  == Ok False
    -- ?debug=1      == Err (Invalid "1")
    -- ?debug=0      == Err (Invalid "0")

You could add `0` and `1` to the dictionary if you want to handle those as
well. You can also use [`map`](#map) to say `map (Result.withDefault False) debug`
to get a parser of type `Parser Bool` that swallows any errors and defaults to
`False`.

**Note:** Parameters like `?debug` with no `=` are not supported by this library.
-}
enum : String -> Dict String a -> Parser (Result Problem a)
enum key dict =
  custom key <| \stringList ->
    case stringList of
      [] ->
        Err NotFound

      [string] ->
        case Dict.get string dict of
          Nothing ->
            Err (Invalid string)

          Just value ->
            Ok value

      _ ->
        Err (TooMany stringList)


{-| Create a custom query parser. The [`string`](#string), [`int`](#int), and
[`enum`](#enum) parsers are defined using this function. It can help you handle
anything though!

Say you are unlucky enough to need to handle `?post=2&post=7` to show a couple
posts on screen at once. You could say:

    posts : Parser (Maybe (List Int))
    posts =
      custom "post" (List.maybeMap String.toInt)
-}
custom : String -> (List String -> a) -> Parser a
custom key func =
  Parser \parameters ->
    case Dict.get key parameters of
      Nothing ->
        ( func [], parameters )

      Just (_, stringList) ->
        ( func stringList
        , Dict.insert key (Visited, stringList) parameters
        )



-- MAPPING


{-| Transform a parser in some way. Maybe you want your `page` query parser to
default to `1` if there is any problem?

    page : Parser Int
    page =
      map (Result.withDefault 1) (int "page")

-}
map : (a -> b) -> Parser a -> Parser b
map func (Parser pA) =
  Parser <| \params0 ->
    let
      ( a, params1 ) = pA params0
    in
      ( func a, params1 )


{-| Combine two parsers. A query like `?search=hats&page=2` could be parsed
with something like this:

    type alias Query =
      { search : Result Problem String
      , page : Result Problem Int
      }

    query : Parser Query
    query =
      map2 Query (string "search") (int "page")

-}
map2 : (a -> b -> result) -> Parser a -> Parser b -> Parser result
map2 func (Parser pA) (Parser pB) =
  Parser <| \params0 ->
    let
      ( a, params1 ) = pA params0
      ( b, params2 ) = pB params1
    in
      ( func a b, params2 )


{-| Combine three parsers. A query like `?search=hats&page=2&sort=ascending`
could be parsed with something like this:

    import Dict

    type alias Query =
      { search : Result Problem String
      , page : Result Problem Int
      , sort : Result Problem Order
      }

    type Order = Ascending | Descending

    query : Parser Query
    query =
      map3 Query (string "search") (int "page") (enum "sort" order)

    order : Dict.Dict String Order
    order =
      Dict.fromList
        [ ( "ascending", Ascending )
        , ( "descending", Descending )
        ]
-}
map3 : (a -> b -> c -> result) -> Parser a -> Parser b -> Parser c -> Parser result
map3 func (Parser pA) (Parser pB) (Parser pC) =
  Parser <| \params0 ->
    let
      ( a, params1 ) = pA params0
      ( b, params2 ) = pB params1
      ( c, params3 ) = pC params2
    in
      ( func a b c, params3 )


{-|-}
map4 : (a -> b -> c -> d -> result) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser result
map4 func (Parser pA) (Parser pB) (Parser pC) (Parser pD) =
  Parser <| \params0 ->
    let
      ( a, params1 ) = pA params0
      ( b, params2 ) = pB params1
      ( c, params3 ) = pC params2
      ( d, params4 ) = pD params3
    in
      ( func a b c d, params4 )


{-|-}
map5 : (a -> b -> c -> d -> e -> result) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser result
map5 func (Parser pA) (Parser pB) (Parser pC) (Parser pD) (Parser pE) =
  Parser <| \params0 ->
    let
      ( a, params1 ) = pA params0
      ( b, params2 ) = pB params1
      ( c, params3 ) = pC params2
      ( d, params4 ) = pD params3
      ( e, params5 ) = pE params4
    in
      ( func a b c d e, params5 )


{-|-}
map6 : (a -> b -> c -> d -> e -> f -> result) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser result
map6 func (Parser pA) (Parser pB) (Parser pC) (Parser pD) (Parser pE) (Parser pF) =
  Parser <| \params0 ->
    let
      ( a, params1 ) = pA params0
      ( b, params2 ) = pB params1
      ( c, params3 ) = pC params2
      ( d, params4 ) = pD params3
      ( e, params5 ) = pE params4
      ( f, params6 ) = pF params5
    in
      ( func a b c d e f, params6 )


{-|-}
map7 : (a -> b -> c -> d -> e -> f -> g -> result) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser result
map7 func (Parser pA) (Parser pB) (Parser pC) (Parser pD) (Parser pE) (Parser pF) (Parser pG) =
  Parser <| \params0 ->
    let
      ( a, params1 ) = pA params0
      ( b, params2 ) = pB params1
      ( c, params3 ) = pC params2
      ( d, params4 ) = pD params3
      ( e, params5 ) = pE params4
      ( f, params6 ) = pF params5
      ( g, params7 ) = pG params6
    in
      ( func a b c d e f g, params7 )


{-| If you need higher than eight, you can define a function like this:

    apply : Parser a -> Parser (a -> b) -> Parser b
    apply argParser funcParser =
      map2 (<|) funcParser argParser

And then you can chain it to do as many of these as you would like:

    map func (string "search")
      |> apply (int "page")
      |> apply (int "per-page")

-}
map8 : (a -> b -> c -> d -> e -> f -> g -> h -> result) -> Parser a -> Parser b -> Parser c -> Parser d -> Parser e -> Parser f -> Parser g -> Parser h -> Parser result
map8 func (Parser pA) (Parser pB) (Parser pC) (Parser pD) (Parser pE) (Parser pF) (Parser pG) (Parser pH) =
  Parser <| \params0 ->
    let
      ( a, params1 ) = pA params0
      ( b, params2 ) = pB params1
      ( c, params3 ) = pC params2
      ( d, params4 ) = pD params3
      ( e, params5 ) = pE params4
      ( f, params6 ) = pF params5
      ( g, params7 ) = pG params6
      ( h, params8 ) = pH params7
    in
      ( func a b c d e f g h, params8 )



-- PARSE


parse : Parser a -> String -> a
parse (Parser parser) input =
  Tuple.first (parser (toParameters input))


parseWithLeftovers : Parser a -> String -> { query : a, leftovers : Dict String (List String) }
parseWithLeftovers (Parser parser) input =
  let
    (query, parameters) =
      parser (toParameters input)
  in
    { query = query
    , leftovers = Dict.foldl keepVisited Dict.empty parameters
    }


keepVisited : String -> (Status, a) -> Dict String a -> Dict String a
keepVisited key ( status, value ) dict =
  case status of
    Unvisited ->
      Dict.insert key value dict

    Visited ->
      dict


toParameters : String -> Parameters
toParameters input =
  let
    query =
      if String.left 1 input /= "?" then
        String.dropLeft 1 input
      else
        input
  in
    List.foldr addToParameters Dict.empty (String.split "&" query)


addToParameters : String -> Parameters -> Parameters
addToParameters segment dict =
  case String.split "=" segment of
    [rawKey, rawValue] ->
      case Maybe.map2 (,) (percentDecode rawKey) (percentDecode rawValue) of
        Nothing ->
          dict

        Just (key, value) ->
          Dict.modify key (addToParametersHelp value) dict

    _ ->
      dict


addToParametersHelp : a -> Maybe (Status, List a) -> Maybe (Status, List a)
addToParametersHelp value maybeList =
  case maybeList of
    Nothing ->
      Just ( Unvisited, [value] )

    Just list ->
      Just ( Unvisited, value :: list )
