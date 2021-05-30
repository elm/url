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
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }



-- MODEL


type alias Model =
    { history : List (Maybe Route)
    , key : Nav.Key
    }


init : () -> Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( Model [ P.parse routeParser url ] key
    , Cmd.none
    )



-- URL PARSING


type Route
    = Home
    | BlogList (Maybe String)
    | BlogPost Int


routeParser : Parser (Route -> a) a
routeParser =
    P.oneOf
        [ P.map Home top
        , P.map BlogList (s "blog" <?> Q.string "search")
        , P.map BlogPost (s "blog" </> P.int)
        ]



-- UPDATE


type Msg
    = UrlChange Url
    | UrlRequest Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange url ->
            ( { model | history = P.parse routeParser url :: model.history }
            , Cmd.none
            )

        UrlRequest request ->
            case request of
                Browser.Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                Browser.External url ->
                    ( model
                    , Nav.load url
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
    case maybeRoute of
        Nothing ->
            li [] [ code [] [ text "Unknown URL" ] ]

        Just route ->
            li [] [ code [] [ text (Debug.toString route) ] ]
