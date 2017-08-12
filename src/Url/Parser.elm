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

    parse string "/alice/" == Just "alice"
    parse string "/bob"    == Just "bob"
    parse string "/42/"    == Just "42"
-}
string : Parser (String -> a) a
string =
  custom "STRING" Just


{-| Parse a segment of the path as an `Int`.

    parse int "/alice/" == Nothing
    parse int "/bob"    == Nothing
    parse int "/42/"    == Just 42
-}
int : Parser (Int -> a) a
int =
  custom "NUMBER" String.toInt


{-| Parse a segment of the path if it matches a given string. It is almost
always used with [`</>`](#</>) or [`oneOf`](#oneOf). For example:

    parse (s "blog" </> int) "/blog/42" == Just 42
    parse (s "blog" </> int) "/tree/42" == Nothing

The path segment must be an exact match!
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
`int` parser:

    int =
      custom "NUMBER" String.toInt

You can use it to define something like “only CSS files” like this:

    css : Parser (String -> a) a
    css =
      custom "CSS_FILE" <| \segment ->
        if String.endsWith ".css" segment then
          Just segment
        else
          Nothing
-}
custom : String -> (String -> Maybe a) -> Parser (a -> b) b
custom tipe stringToSomething =
  Parser <| \{ visited, unvisited, params, value } ->
    case unvisited of
      [] ->
        []

      next :: rest ->
        case stringToSomething next of
          Just nextValue ->
            [ State (next :: visited) rest params (value nextValue) ]

          Nothing ->
            []



-- COMBINING PARSERS


{-| Parse a path with multiple segments.

    blog : Parser (Int -> a) a
    blog =
      s "blog" </> int

    -- parse blog "/blog/35/"  ==  Just 35
    -- parse blog "/blog/42"   ==  Just 42
    -- parse blog "/blog/"     ==  Nothing
    -- parse blog "/42/"       ==  Nothing

    search : Parser (String -> a) a
    search =
      s "search" </> string

    -- parse search "/search/cats/"  ==  Just "cats"
    -- parse search "/search/frog"   ==  Just "frog"
    -- parse search "/search/"       ==  Nothing
    -- parse search "/cats/"         ==  Nothing
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

    -- parse comment "/user/bob/comments/42" == Just { author = "bob", id = 42 }
    -- parse comment "/user/tom/comments/35" == Just { author = "tom", id = 35 }
    -- parse comment "/user/sam/"            == Nothing
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

    -- parse route "/search/cats "         == Just (Search "cats")
    -- parse route "/search/"              == Nothing

    -- parse route "/blog/42"              == Just (Blog 42)
    -- parse route "/blog/cats"            == Nothing

    -- parse route "/user/sam/"            == Just (User "sam")
    -- parse route "/user/bob/comments/42" == Just (Comment "bob" 42)
    -- parse route "/user/tom/comments/35" == Just (Comment "tom" 35)
    -- parse route "/user/"                == Nothing

If there are multiple parsers that could succeed, the first one wins.
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
        , map Post (s "post" </> int)
        ]

    -- parse (s "blog" </> blogRoute) "/blog/"        == Just Overview
    -- parse (s "blog" </> blogRoute) "/blog/post/42" == Just (Post 42)
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


{-| Run a parser!
-}
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
