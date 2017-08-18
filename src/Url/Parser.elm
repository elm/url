module Url.Parser exposing
  ( Parser, string, int, s
  , (</>), map, oneOf, top, custom
  , (<?>), query
  , fragment
  , parse
  , parsePath, Url
  , parseSegments
  , preparePath, prepareQuery, prepareFragment
  )

{-| In [the URI spec](https://tools.ietf.org/html/rfc3986), Tim Berners-Lee
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

# Path
@docs (</>), map, oneOf, top, custom

# Query
@docs (<?>), query

# Fragment
@docs fragment

# Run Parsers
@docs parse, parsePath, Url

# Low-Level Stuff
@docs parseSegments, preparePath, prepareQuery, prepareFragment

-}

import Dict exposing (Dict)
import Url exposing (percentDecode)
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
  , query : Dict String (List String)
  , fragment : Maybe String
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
  Parser <| \{ visited, unvisited, query, fragment, value } ->
    case unvisited of
      [] ->
        []

      next :: rest ->
        if next == str then
          [ State (next :: visited) rest query fragment value ]

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
  Parser <| \{ visited, unvisited, query, fragment, value } ->
    case unvisited of
      [] ->
        []

      next :: rest ->
        case stringToSomething next of
          Just nextValue ->
            [ State (next :: visited) rest query fragment (value nextValue) ]

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
  Parser <| \{ visited, unvisited, query, fragment, value } ->
    List.map (mapState value) <| parse <|
      State visited unvisited query fragment subValue


mapState : (a -> b) -> State a -> State b
mapState func { visited, unvisited, query, fragment, value } =
  State visited unvisited query fragment (func value)


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
  Parser <| \{ visited, unvisited, query, fragment, value } ->
    [ State visited unvisited query fragment (value (queryParser query))
    ]



-- FRAGMENT


{-| Create a parser for the URL fragment, the stuff after the `#`. This can
be handy for handling links to DOM elements within a page. Pages like this one!

    type alias Docs =
      { moduleName : String
      , value : Maybe String
      }

    docs : Parser (Docs -> a) a
    docs =
      map Docs (string </> fragment identity)

    -- parse docs "/List/#map" == Just (Docs "List" (Just "map"))
    -- parse docs "/List#map"  == Just (Docs "List" (Just "map"))
    -- parse docs "/List#"     == Just (Docs "List" (Just ""))
    -- parse docs "/List"      == Just (Docs "List" Nothing)

-}
fragment : (Maybe String -> fragment) -> Parser (fragment -> a) a
fragment toFrag =
  Parser <| \{ visited, unvisited, query, fragment, value } ->
    [ State visited unvisited query fragment (value (toFrag fragment))
    ]



-- PARSE


{-| Actually run a parser! Remember how at the beginning of the docs I said
[the URI spec](https://tools.ietf.org/html/rfc3986) defines a URL like this:

```
  https://example.com:8042/over/there?name=ferret#nose
  \___/   \______________/\_________/ \_________/ \__/
    |            |            |            |        |
  scheme     authority       path        query   fragment
```

The `parse` function is expecting the `path`, `query`, and `fragment`.
Rules include:

  - No `scheme`
  - No `authority`
  - The path can start with a `/` or not. It does not matter.
  - The path can end with a `/` or not. It does not matter.
  - All query parameters must have an `=`, like `key=value`.
  - If there is more than one `?` or `#` it fails.

-}
parse : Parser (a -> a) a -> String -> Maybe a
parse parser pathQueryFragment =
  case String.indexes "?" pathQueryFragment of
    [i] ->
      case String.indexes "#" pathQueryFragment of
        [j] ->
          parseSegments
            parser
            (preparePath (String.left i pathQueryFragment))
            (prepareQuery (String.slice i j pathQueryFragment))
            (prepareFragment (String.dropLeft j pathQueryFragment))

        _ ->
          Nothing

    _ ->
      Nothing


{-| Parse a [`Url`][url] in a single-page app. You provide (1) a parser to
convert the URL to nice data and (2) a function to handle failure.

    import Url.Parser exposing (Parser, Url, int, map, oneOf, parsePath, s, top)

    type Route = Home | Blog Int | NotFound String

    route : Parser (Route -> a) a
    route =
      oneOf
        [ map Home top
        , map Blog (s "blog" </> int)
        ]

    parseRoute : Url -> Route
    parseRoute url =
      parsePath route NotFound url

    -- /                ==>  Home
    -- /blog            ==>  NotFound "/blog"
    -- /blog/42         ==>  Blog 42
    -- /blog/42/        ==>  Blog 42
    -- /blog/42#cats    ==>  Blog 42
    -- /blog/42?q=cats  ==>  Blog 42
    -- /settings        ==>  NotFound "/settings"

[url]: http://package.elm-lang.org/packages/elm-lang/browser/latest/Browser#Url

-}
parsePath : Parser (a -> a) a -> (String -> a) -> Url -> a
parsePath parser notFound { pathname, search, hash } =
  case parseSegments parser (preparePath pathname) (prepareQuery search) (prepareFragment hash) of
    Just answer ->
      answer

    Nothing ->
      notFound (pathname ++ search ++ hash)


{-| This matches the URL type from [`elm-lang/browser`][browser] package.
It makes it easier to use [`parsePath`](#parsePath) to manage routing in a
single-page app.

[browser]: http://package.elm-lang.org/packages/elm-lang/browser/latest

**Note:** The fields correspond with the fields in `document.location` as
described [here](https://developer.mozilla.org/en-US/docs/Web/API/Url).
-}
type alias Url =
  { href : String
  , host : String
  , hostname : String
  , protocol : String
  , origin : String
  , port_ : String
  , pathname : String
  , search : String
  , hash : String
  , username : String
  , password : String
  }



-- LOW-LEVEL PARSE


{-| **Prefer to use [`parse`](#parse) or [`parsePath`](#parsePath) instead of
this function!**

The `parseSegments` function is the core URL parser. It is used to define the
more convenient `parse` and `parsePath` functions. I decided to expose
`parseSegments` not because I think folks should be using it directly, but
because I think it makes the documentation a bit more helpful!

In particular, you need to use [`preparePath`](#preparePath),
[`prepareQuery`](#prepareQuery), and [`prepareFragment`](#prepareFragment) to
prepare the arguments to `parseSegments`. Those functions have a bunch of
documentation, so you can answer questions like “What if I have an extra `/`
on my path?” more easily.
-}
parseSegments : Parser (a -> a) a -> List String -> Dict String (List String) -> Maybe String -> Maybe a
parseSegments (Parser parser) path query fragment =
  getFirstMatch <| parser <|
    State [] path query fragment identity


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



-- PREPARE PATH


{-| When using [`parseSegments`](#parseSegments), this function helps get the
path in the right format:

    preparePath "/blog/42/" == [ "blog", "42" ]
    preparePath "/blog/42"  == [ "blog", "42" ]
    preparePath "blog/42"   == [ "blog", "42" ]

Notice that having a `/` at the beginning or the end does not change the
output. This means `/a/b` and `/a/b/` will be the same URL with this function.
-}
preparePath : String -> List String
preparePath path =
  case String.split "/" path of
    "" :: segments ->
      removeFinalEmpty segments

    segments ->
      removeFinalEmpty segments


removeFinalEmpty : List String -> List String
removeFinalEmpty segments =
  case segments of
    [] ->
      []

    "" :: [] ->
      []

    segment :: rest ->
      segment :: removeFinalEmpty rest



-- PREPARE QUERY


{-| When using [`parseSegments`](#parseSegments), this function helps get the
query string in the right format:

    import Dict

    prepareQuery "search=cat"  == Dict.fromList [ ("search", ["cat"]) ]
    prepareQuery "?search=cat" == Dict.fromList [ ("search", ["cat"]) ]
    prepareQuery "?symbol=%23" == Dict.fromList [ ("symbol", ["#"]) ]

    prepareQuery "?x=3&y=4"    == Dict.fromList [ ("x", ["3"]), ("y", ["4"]) ]
    prepareQuery "?x=3&x=4"    == Dict.fromList [ ("x", ["3","4"]) ]
    prepareQuery "?debug&x=3"  == Dict.fromList [ ("x", ["3"]) ]

Notice that:

  - Skipping the `?` at the beginning is allowed.
  - [`percentDecode`][pd] is called on the `k` and `v` parts of a `k=v` pair.
  - When there are repeat parameters, their order is maintained.
  - Messed up parameters (e.g. no `=`) are skipped.

[pd]: http://package.elm-lang.org/packages/elm-lang/url/latest/Url-Builder#percentDecode
-}
prepareQuery : String -> Dict String (List String)
prepareQuery query =
  List.foldr addParam Dict.empty (String.split "&" query)


addParam : String -> Dict String (List String) -> Dict String (List String)
addParam segment dict =
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



-- PREPARE FRAGMENT


{-| When using [`parseSegments`](#parseSegments), this function helps get the
hash in the right format:

    prepareFragment ""        == Nothing
    prepareFragment "no-hash" == Nothing
    prepareFragment "#"       == Nothing
    prepareFragment "#hi"     == Just "hi"
    prepareFragment "#hello"  == Just "hello"

Notice that no `#` means you get `Nothing`.

[loc]: http://package.elm-lang.org/packages/elm-lang/navigation/latest/Navigation#Location
-}
prepareFragment : String -> Maybe String
prepareFragment fragment =
  if String.startsWith "#" fragment then
    case String.dropLeft 1 fragment of
      "" ->
        Nothing

      hash ->
        Just hash

  else
    Nothing
