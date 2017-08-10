module Url.Builder exposing
  ( url
  , percentEncode
  , percentDecode
  )


{-|

# Percent-Encoding
@docs percentEncode, percentDecode

-}



url : String -> List (String,String) -> String
url baseUrl args =
  case args of
    [] ->
        baseUrl

    _ ->
        baseUrl ++ "?" ++ String.join "&" (List.map queryPair args)


queryPair : (String,String) -> String
queryPair (key,value) =
  percentEncode key ++ "=" ++ percentEncode value


{-| Encode a string to be placed in any part of a URI. Same behavior as
JavaScript's `encodeURIComponent` function.
-}
percentEncode : String -> String
percentEncode =
  Native.Http.percentEncode


{-| Decode a URI string. Same behavior as JavaScript's `decodeURIComponent`
function.
-}
percentDecode : String -> String
percentDecode =
  Native.Http.percentDecode