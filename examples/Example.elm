module Main exposing (Model, Msg(..), Route(..), init, main, onUrlChange, onUrlRequest, route, routeToString, subscriptions, update, view, viewLink, viewRoute)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation exposing (Key, pushUrl)
import Html exposing (Html, a, button, code, div, h1, h3, li, text, ul)
import Html.Attributes exposing (href)
import Html.Events exposing (onClick)
import Url exposing (Url)
import Url.Parser exposing ((</>), (<?>), Parser, s, top)
import Url.Parser.Query


main : Program () Model Msg
main =
    Browser.application
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = onUrlRequest
        , onUrlChange = onUrlChange
        }



-- MODEL


type alias Model =
    { history : List (Maybe Route)
    , key : Key
    }


init : Url -> Key -> ( Model, Cmd Msg )
init url key =
    ( Model [ Url.Parser.parse route url ] key
    , Cmd.none
    )



-- URL PARSING


type Route
    = Home
    | BlogList (Maybe String)
    | BlogPost Int


route : Parser (Route -> a) a
route =
    Url.Parser.oneOf
        [ Url.Parser.map Home top
        , Url.Parser.map BlogList (s "blog" <?> Url.Parser.Query.string "search")
        , Url.Parser.map BlogPost (s "blog" </> Url.Parser.int)
        ]



-- UPDATE


type Msg
    = NewUrl String
    | UrlChange Url
    | Updates


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewUrl url ->
            ( model
            , pushUrl model.key url
            )

        UrlChange location ->
            ( { model | history = Url.Parser.parse route location :: model.history }
            , Cmd.none
            )

        Updates ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- UPDATES


onUrlRequest : UrlRequest -> Msg
onUrlRequest req =
    case req of
        Internal url_ ->
            Debug.log ("URL requested for internal at " ++ Url.toString url_) Updates

        External str ->
            Debug.log ("Some updates related to" ++ str) Updates


onUrlChange : Url -> Msg
onUrlChange url =
    UrlChange url



-- VIEW


view : Model -> Document Msg
view model =
    Document "Example Page for elm/url"
        [ div []
            [ h1 [] [ text "Links" ]
            , ul [] (List.map viewLink [ "/", "/blog/", "/blog/42", "/blog/37", "/blog/?search=cats" ])
            , h1 [] [ text "History" ]
            , ul [] (List.map viewRoute model.history)
            , h3 [] [ a [ href "http://example.com" ] [ text "External Link" ] ]
            , h3 [] [ a [ href "/something" ] [ text "Internal Link" ] ]
            ]
        ]


viewLink : String -> Html Msg
viewLink url =
    li [] [ button [ onClick (NewUrl url) ] [ text url ] ]


viewRoute : Maybe Route -> Html msg
viewRoute maybeRoute =
    case maybeRoute of
        Nothing ->
            li [] [ text "Invalid URL" ]

        Just route_ ->
            li [] [ code [] [ text (routeToString route_) ] ]


routeToString : Route -> String
routeToString route_ =
    case route_ of
        Home ->
            "home"

        BlogList Nothing ->
            "list all blog posts"

        BlogList (Just search) ->
            "search for " ++ search

        BlogPost id ->
            "show blog " ++ String.fromInt id
