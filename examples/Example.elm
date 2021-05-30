module Example exposing (main)


import Browser
import Browser.Navigation as Nav
import Html exposing (Html, a, code, div, h1, h3, li, text, ul)
import Html.Attributes exposing (href)
import Url exposing (Url)
import Url.Parser as P exposing (Parser, (</>), (<?>), s, top)
import Url.Parser.Query as Q



-- MAIN


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequest         -- This should be a function of type `Browser.UrlRequest -> Msg`.
                                            -- It isn't a handler like in JS, but a message that `update` can
                                            -- process. 
        , onUrlChange = UrlChange           -- Similarly, this is also a function that returns a message. However,
                                            -- it accepts a `Url`, not a `Browser.UrlRequest`.
        }



-- MODEL


type alias Model =
    { history : List (Maybe Route)      -- Keep track of the routes. The Maybe is because parsing can (and will) fail. 
    , key : Nav.Key
    }


-- Nav.Key is needed for controlling navigation, it is provided by `init` and we store it in the `Model`
-- If you want to understand what its actually doing, the docs for it are linked below:
-- 

init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model [ P.parse routeParser url ] key
    , Cmd.none
    )



-- URL PARSING


type Route                      -- A custom type to describe a route. This is how our parser will give us its results. 
    = Home                      -- "/"
    | BlogList (Maybe String)   -- "/blog?search=..." (`Just ...`) or "/blog" (`Nothing`)
    | BlogPost Int              -- "/blog/<integer>"

-- A parser is built by starting with simple builtin parsers, like "the root path" or "this directory",
-- and then these parsers are combined with *parser combinators*, like </> and <?>, which are explained
-- more concretely and in greater detail below. 
routeParser : Parser (Route -> a) a
routeParser =
    P.oneOf                                                 -- This is the first parser combinator, it takes a list
                                                            -- of parsers, and if one of the parsers fails, it just
                                                            -- tries the next one given. The URL parser it returns
                                                            -- can go through all of the different patterns specified
                                                            -- and generate the correct representation of the URL. 

        [ P.map Home top                                    -- `top : Parser a a` and `s : String -> Parser a a`
                                                            -- are the two main "starting points" for building a
                                                            -- basic parser. top represents the root page "/",
                                                            -- and s is used to represent a particular directory.
                                                            
                                                            -- Just like Maybe.map allows you to have a function
                                                            -- run in the context of the Maybe, P.map allows us to
                                                            -- insert a function "into" the parser in a similar manner.
                                                            -- Here, we are using the type constructor `Home` to turn
                                                            -- basic `top` parser into one that, when given a matching
                                                            -- URL ("/" in this case), will return Home. 

        , P.map BlogList (s "blog" <?> Q.string "search")   -- The parser combinator <?> is used to combine a
                                                            -- standard URL parser (Parser ...) with a
                                                            -- query string parser (Q.Parser ...). 

                                                            -- Here, the `Q.string : String -> Q.Parser (Maybe String)`
                                                            -- function is used as a basic query string parser.
                                                            -- `Q.String` will parse the given query parameter, so this
                                                            -- is going to extract the query parameter "search".
                                                            -- Note that when mapping over a parser created with <?>,
                                                            -- the the value passed into the mapping function (here
                                                            -- `BlogList`), will be the result from the query parser.
                                                            -- Since `Q.String "..."` is a `Q.Parser (Maybe String)`,
                                                            -- the mapping function should take a `Maybe String`.

        , P.map BlogPost (s "blog" </> P.int)               -- Simply converting the directory name to an int
                                                            -- wouldn't always work, and we would have to handle the
                                                            -- Maybe ourselves. Instead of having to deal with a
                                                            -- `BlogPost (Maybe Int)`, or having to get the parser to
                                                            -- fail on non-integers ourselves, we can use the builtin
                                                            -- P.int do handle the directory name as an integer.
                                                            -- Note that this is in contrast to the standard query
                                                            -- parser, where the parser returns a `Maybe String`.
                                                            -- There, not finding a particular query parameter is not
                                                            -- indicative of failure, but just is something should be
                                                            -- dealt with as an optional value.  

                                                            -- The parser combinator </> is used just like <?>,
                                                            -- but is used to combine two URL parsers, where
                                                            -- instead of the second parsing query parameters after
                                                            -- the first, it will parse subdirectories found after
                                                            -- the first. 
        ]

-- You could use `Debug.toString : a -> String` here, but that is not recommended for production use
routeToString : Route -> String
routeToString route =
    case route of
        BlogList (Just name) ->
            "BlogList (Just " ++ name ++ ")"

        BlogList Nothing ->
            "BlogList Nothing"

        BlogPost num ->
            "BlogPost " ++ String.fromInt num

        Home ->
            "Home"



-- UPDATE


type Msg
    = UrlChange Url
    | UrlRequest Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange url ->
            ( { model | history = P.parse routeParser url :: model.history }    -- The main part of this is,
                                                                                -- `P.parse routeParser url`,
                                                                                -- where we are parsing the URL
                                                                                -- with the parser we just built.
                                                                                -- Then, we use the `::` operator
                                                                                -- to add it to the history list.
            , Cmd.none
            )

        UrlRequest request ->
            case request of             -- onUrlRequest doesn't actually provide a Url, but a Browser.UrlRequest. 
                                        -- It isn't just a URL, it needs to carry some additional information. 
                                        -- It is explained on the docs for Browser.UrlRequest, here:
                                        -- https://package.elm-lang.org/packages/elm/browser/latest/Browser#UrlRequest
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)      -- Change the URL, but do not trigger a page load.
                                                                    -- Adds a new entry to the browser history.
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url                                  -- Basically a redirect (always a page load)
                    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    Browser.Document "Example Page for elm/url"
        [ div []
            [ h1 [] [ text "Links" ]
            , ul [] (List.map viewLink [ "/", "/blog/", "/blog/42", "/blog/37", "/blog/?search=cats" ])
            , h1 [] [ text "History" ]
            , ul [] (List.map viewRoute model.history)
            ]
        ]


viewLink : String -> Html Msg
viewLink url =
    li [] [ a [ href url ] [ text url ] ]


viewRoute : Maybe Route -> Html msg
viewRoute maybeRoute =
    case maybeRoute of                                  -- URL parsing can succeed or fail, we must handle both cases. 
        Nothing ->
            li [] [ code [] [ text "Unknown URL" ] ]

        Just route ->
            li [] [ code [] [ text (routeToString route) ] ]
