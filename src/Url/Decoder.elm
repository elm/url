module Url.Decoder exposing (url)

import Json.Decode as D
import Url

url : D.Decoder Url.Url
url =
    D.string |> D.andThen checkUrl

checkUrl : String -> D.Decoder Url.Url
checkUrl string =
    case Url.fromString string of
        Nothing ->
            D.fail "Not a valid URL"

        Just actualUrl ->
            D.succeed actualUrl
