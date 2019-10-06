module Api.Api exposing (requestWithPayload, requestWithoutPayload)

import Base64
import Crypto.HMAC as HMAC
import Http
import Json.Encode as Encode
import String.Conversions as Conversions


signature : String -> String -> String
signature apiSecret stringToSign =
    HMAC.digest HMAC.sha384 apiSecret stringToSign


requestWithoutPayload : String -> String -> String -> String -> String -> Http.Expect msg -> String -> Cmd msg
requestWithoutPayload nonce apiKey apiSecret host endpoint expect method =
    let
        signature_ =
            [ method, endpoint, nonce ]
                |> String.join " "
                |> signature apiSecret
    in
    Http.request
        { method = method
        , headers =
            [ Http.header "X-SBTC-APIKEY" apiKey
            , Http.header "X-SBTC-NONCE" nonce
            , Http.header "X-SBTC-SIGNATURE" signature_
            ]
        , body = Http.emptyBody
        , url = host ++ endpoint
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


requestWithPayload : String -> String -> String -> String -> String -> String -> Http.Expect msg -> String -> Cmd msg
requestWithPayload nonce apiKey apiSecret host payload endpoint expect method =
    let
        base64EncodedBody =
            payload |> Base64.encode

        signature_ =
            [ method, endpoint, base64EncodedBody, nonce ]
                |> String.join " "
                |> signature apiSecret
    in
    Http.request
        { method = method
        , headers =
            [ Http.header "X-SBTC-APIKEY" apiKey
            , Http.header "X-SBTC-NONCE" nonce
            , Http.header "X-SBTC-SIGNATURE" signature_
            ]
        , body = Http.stringBody "application/json" payload
        , url = host ++ endpoint
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }
