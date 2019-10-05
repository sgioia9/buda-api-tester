module Api.Api exposing (authenticatedGet, authenticatedPost)

import Base64
import Crypto.HMAC as HMAC
import Http
import Json.Encode as Encode
import String.Conversions as Conversions


stringToSignWithBody : String -> String -> String -> String -> String
stringToSignWithBody httpMethod endpoint base64body nonce =
    httpMethod ++ " " ++ endpoint ++ " " ++ base64body ++ " " ++ nonce


stringToSignWithoutBody : String -> String -> String -> String
stringToSignWithoutBody httpMethod endpoint nonce =
    httpMethod ++ " " ++ endpoint ++ " " ++ nonce


signature : String -> String -> String
signature apiKey stringToSign =
    HMAC.digest HMAC.sha384 apiKey stringToSign


authenticatedGet : String -> String -> String -> String -> Http.Expect msg -> Cmd msg
authenticatedGet nonce apiKey host endpoint expect =
    let
        signature_ =
            stringToSignWithoutBody "GET" endpoint nonce |> signature apiKey
    in
    Http.request
        { method = "GET"
        , headers =
            [ Http.header "X-SBTC-APIKEY" apiKey
            , Http.header "X-SBTC-NONCE" nonce
            , Http.header "X-SBTC-SIGNATURE" signature_
            ]
        , body = Http.emptyBody
        , url = host ++ "/" ++ endpoint
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


authenticatedPost : String -> String -> String -> Encode.Value -> String -> Http.Expect msg -> Cmd msg
authenticatedPost nonce apiKey host json endpoint expect =
    let
        base64EncodedBody =
            Conversions.fromValue json |> Base64.encode

        signature_ =
            stringToSignWithBody "POST" endpoint base64EncodedBody nonce |> signature apiKey
    in
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "X-SBTC-APIKEY" apiKey
            , Http.header "X-SBTC-NONCE" nonce
            , Http.header "X-SBTC-SIGNATURE" signature_
            ]
        , body = Http.jsonBody json
        , url = host ++ "/" ++ endpoint
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }
