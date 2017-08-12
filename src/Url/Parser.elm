module Url.Parser exposing
  ( Parser, string, int, s
  , (</>), map, oneOf, top, custom
  , (<?>), query
  , parse
  )

{-| In [the URI spec](https://tools.ietf.org/html/rfc3986), Tim Breners-Lee
says a URL looks like this:

```
  https://example.com:8042/over/there?name=ferret#nose
  \___/   \______________/\_________/ \_________/ \__/
    |            |            |            |        |
  scheme     authority       path        query   fragment
```

This module is for parsing the `path` part.


# Primitives
@docs Parser, string, int, s

# Parse the Path
@docs (</>), map, oneOf, top, custom

# Parse the Query
@docs (<?>), query

# Run a Parser
@docs parse

-}

import Dict exposing (Dict)
import Url.Builder exposing (percentDecode)
import Url.Parser.Query as Query
import Url.Parser.Internal as Q



-- INFIX TABLE


infixr 7 </>
infixl 8 <?>



-- PARSERS


{-| Turn URLs like `/blog/42/cat-herding-techniques` into nice Elm data.
-}
type Parser a b =
  Parser (State a -> List (State b))


type alias State value =
  { visited : List String
  , unvisited : List String
  , params : Dict String (List String)
  , value : value
  }



-- PARSE SEGMENTS


{-| Parse a segment of the path as a `String`.

    parsePath string location
    -- /alice/  ==>  Just "alice"
    -- /bob     ==>  Just "bob"
    -- /42/     ==>  Just "42"
-}
string : Parser (String -> a) a
string =
  custom "STRING" Ok


{-| Parse a segment of the path as an `Int`.

    parsePath int location
    -- /alice/  ==>  Nothing
    -- /bob     ==>  Nothing
    -- /42/     ==>  Just 42
-}
int : Parser (Int -> a) a
int =
  custom "NUMBER" String.toInt


{-| Parse a segment of the path if it matches a given string.

    s "blog"  -- can parse /blog/
              -- but not /glob/ or /42/ or anything else
-}
s : String -> Parser a a
s str =
  Parser <| \{ visited, unvisited, params, value } ->
    case unvisited of
      [] ->
        []

      next :: rest ->
        if next == str then
          [ State (next :: visited) rest params value ]

        else
          []


{-| Create a custom path segment parser. Here is how it is used to define the
`int` and `string` parsers:

    int =
      custom "NUMBER" String.toInt

    string =
      custom "STRING" Ok

You can use it to define something like “only CSS files” like this:

    css : Parser (String -> a) a
    css =
      custom "CSS_FILE" <| \segment ->
        if String.endsWith ".css" segment then
          Ok segment
        else
          Err "Does not end with .css"
-}
custom : String -> (String -> Result String a) -> Parser (a -> b) b
custom tipe stringToSomething =
  Parser <| \{ visited, unvisited, params, value } ->
    case unvisited of
      [] ->
        []

      next :: rest ->
        case stringToSomething next of
          Ok nextValue ->
            [ State (next :: visited) rest params (value nextValue) ]

          Err msg ->
            []



-- COMBINING PARSERS


{-| Parse a path with multiple segments.

    parsePath (s "blog" </> int) location
    -- /blog/35/  ==>  Just 35
    -- /blog/42   ==>  Just 42
    -- /blog/     ==>  Nothing
    -- /42/       ==>  Nothing

    parsePath (s "search" </> string) location
    -- /search/cats/  ==>  Just "cats"
    -- /search/frog   ==>  Just "frog"
    -- /search/       ==>  Nothing
    -- /cats/         ==>  Nothing
-}
(</>) : Parser a b -> Parser b c -> Parser a c
(</>) (Parser parseBefore) (Parser parseAfter) =
  Parser <| \state ->
    List.concatMap parseAfter (parseBefore state)


{-| Transform a path parser.

    type alias Comment = { author : String, id : Int }

    rawComment : Parser (String -> Int -> a) a
    rawComment =
      s "user" </> string </> s "comments" </> int

    comment : Parser (Comment -> a) a
    comment =
      map Comment rawComment

    parsePath comment location
    -- /user/bob/comments/42  ==>  Just { author = "bob", id = 42 }
    -- /user/tom/comments/35  ==>  Just { author = "tom", id = 35 }
    -- /user/sam/             ==>  Nothing
-}
map : a -> Parser a b -> Parser (b -> c) c
map subValue (Parser parse) =
  Parser <| \{ visited, unvisited, params, value } ->
    List.map (mapHelp value) <| parse <|
      { visited = visited
      , unvisited = unvisited
      , params = params
      , value = subValue
      }


mapHelp : (a -> b) -> State a -> State b
mapHelp func {visited, unvisited, params, value} =
  { visited = visited
  , unvisited = unvisited
  , params = params
  , value = func value
  }


{-| Try a bunch of different path parsers.

    type Route
      = Search String
      | Blog Int
      | User String
      | Comment String Int

    route : Parser (Route -> a) a
    route =
      oneOf
        [ map Search  (s "search" </> string)
        , map Blog    (s "blog" </> int)
        , map User    (s "user" </> string)
        , map Comment (s "user" </> string </> s "comments" </> int)
        ]

    parsePath route location
    -- /search/cats           ==>  Just (Search "cats")
    -- /search/               ==>  Nothing

    -- /blog/42               ==>  Just (Blog 42)
    -- /blog/cats             ==>  Nothing

    -- /user/sam/             ==>  Just (User "sam")
    -- /user/bob/comments/42  ==>  Just (Comment "bob" 42)
    -- /user/tom/comments/35  ==>  Just (Comment "tom" 35)
    -- /user/                 ==>  Nothing

-}
oneOf : List (Parser a b) -> Parser a b
oneOf parsers =
  Parser <| \state ->
    List.concatMap (\(Parser parser) -> parser state) parsers


{-| A parser that does not consume any path segments.

    type BlogRoute = Overview | Post Int

    blogRoute : Parser (BlogRoute -> a) a
    blogRoute =
      oneOf
        [ map Overview top
        , map Post  (s "post" </> int)
        ]

    parsePath (s "blog" </> blogRoute) location
    -- /blog/         ==>  Just Overview
    -- /blog/post/42  ==>  Just (Post 42)
-}
top : Parser a a
top =
  Parser <| \state -> [state]



-- QUERY


{-| The [`Url.Parser.Query`](Url-Parser-Query) module defines its own
[`Parser`](Url-Parser-Query#Parser) type. This function helps you use those
with normal parsers. For example, maybe you want to add a search feature to
your blog website:

    import Url.Parser.Query as Query

    type Route
      = BlogList (Result Query.Problem String)
      | BlogPost Int

    route : Parser (Route -> a) a
    route =
      oneOf
        [ map BlogList (s "blog" <?> Query.string "search")
        , map BlogPost (s "blog" </> int)
        ]

    -- parse route "/blog/"             == Just (BlogList (Err Query.NotFound))
    -- parse route "/blog/?search=cats" == Just (BlogList (Ok "cats"))
    -- parse route "/blog/42"           == Just (BlogPost 42)
-}
(<?>) : Parser a (query -> b) -> Query.Parser query -> Parser a b
(<?>) parser queryParser =
  parser </> query queryParser


{-| The [`Url.Parser.Query`](Url-Parser-Query) module defines its own
[`Parser`](Url-Parser-Query#Parser) type. This function is a helper to convert
those into normal parsers.

    import Url.Parser.Query as Query

    -- the following expressions are both the same!
    --
    -- s "blog" <?> Query.string "search"
    -- s "blog" </> query (Query.string "search")

This may be handy if you need query parameters but are not parsing any path
segments.
-}
query : Query.Parser query -> Parser (query -> a) a
query (Q.Parser queryParser) =
  Parser <| \{ visited, unvisited, params, value } ->
    [ { visited = visited
      , unvisited = unvisited
      , params = params
      , value = value (queryParser params)
      }
    ]



-- RUN A PARSER


parse : Parser (a -> a) a -> String -> Maybe a
parse (Parser parser) pathAndQuery =
  case String.split "?" pathAndQuery of
    [ path, query ] ->
      getFirstMatch <| parser <|
        { visited = []
        , unvisited = splitPath path
        , params = queryToParameters query
        , value = identity
        }

    _ ->
      Nothing


getFirstMatch : List (State a) -> Maybe a
getFirstMatch states =
  case states of
    [] ->
      Nothing

    state :: rest ->
      case state.unvisited of
        [] ->
          Just state.value

        [""] ->
          Just state.value

        _ ->
          getFirstMatch rest


splitPath : String -> List String
splitPath path =
  case String.split "/" path of
    "" :: segments ->
      segments

    segments ->
      segments


queryToParameters : String -> Dict String (List String)
queryToParameters query =
  List.foldr addToParameters Dict.empty (String.split "&" query)


addToParameters : String -> Dict String (List String) -> Dict String (List String)
addToParameters segment dict =
  case String.split "=" segment of
    [rawKey, rawValue] ->
      case Maybe.map2 (,) (percentDecode rawKey) (percentDecode rawValue) of
        Nothing ->
          dict

        Just (key, value) ->
          Dict.update key (addToParametersHelp value) dict

    _ ->
      dict


addToParametersHelp : a -> Maybe (List a) -> Maybe (List a)
addToParametersHelp value maybeList =
  case maybeList of
    Nothing ->
      Just [value]

    Just list ->
      Just (value :: list)
