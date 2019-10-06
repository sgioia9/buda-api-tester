module Main exposing (Model, Msg(..), init, main, update, view)

import Api.Api as Api
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Encode as Encode
import RemoteData exposing (WebData)
import Time



---- MODEL ----


type HttpMethod
    = GET
    | POST
    | PUT
    | PATCH
    | DELETE


type alias Model =
    { selectedHttpMethod : Maybe HttpMethod
    , formApiKey : String
    , formApiSecret : String
    , formEndpoint : String
    , formMethod : String
    , formJsonPayload : String
    , response : WebData String
    , currentTime : Time.Posix
    }


init : ( Model, Cmd Msg )
init =
    ( { selectedHttpMethod = Just GET
      , formApiKey = ""
      , formApiSecret = ""
      , formEndpoint = "/api/v2/me"
      , formMethod = "GET"
      , formJsonPayload = ""
      , response = RemoteData.NotAsked
      , currentTime = 0 |> Time.millisToPosix
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | FormHttpMethodSelected String
    | FormApiKeyUpdated String
    | FormApiSecretUpdated String
    | FormEndpointUpdated String
    | FormJsonPayloadUpdated String
    | DoRequestButtonPressed
    | GotResponse (WebData String)
    | OneSecondPassed Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        FormHttpMethodSelected selectedMethod ->
            ( { model
                | formMethod = selectedMethod
                , selectedHttpMethod = httpMethodFromString selectedMethod
              }
            , Cmd.none
            )

        FormApiKeyUpdated updatedApiKey ->
            ( { model | formApiKey = updatedApiKey }, Cmd.none )

        FormApiSecretUpdated updatedApiSecret ->
            ( { model | formApiSecret = updatedApiSecret }, Cmd.none )

        FormEndpointUpdated updatedEndpoint ->
            ( { model | formEndpoint = updatedEndpoint }, Cmd.none )

        FormJsonPayloadUpdated updatedPayload ->
            ( { model | formJsonPayload = updatedPayload }, Cmd.none )

        DoRequestButtonPressed ->
            let
                doRequestWithMethod : HttpMethod -> Cmd Msg
                doRequestWithMethod =
                    doRequest
                        model.formApiKey
                        model.formApiSecret
                        (model.currentTime |> Time.posixToMillis |> String.fromInt)
                        model.formEndpoint
                        model.formJsonPayload
            in
            ( { model | response = RemoteData.Loading }
            , model.selectedHttpMethod
                |> Maybe.map doRequestWithMethod
                |> Maybe.withDefault Cmd.none
            )

        GotResponse response ->
            ( { model | response = response }, Cmd.none )

        OneSecondPassed currentTime ->
            ( { model | currentTime = currentTime }, Cmd.none )


doRequest : String -> String -> String -> String -> String -> HttpMethod -> Cmd Msg
doRequest apiKey apiSecret nonce endpoint payload method =
    let
        requestWithPayloadForMethod : String -> Cmd Msg
        requestWithPayloadForMethod =
            Api.requestWithPayload
                nonce
                apiKey
                apiSecret
                "https://stg.buda.com"
                payload
                endpoint
                (Http.expectString (RemoteData.fromResult >> GotResponse))

        requestWithoutPayloadForMethod : String -> Cmd Msg
        requestWithoutPayloadForMethod =
            Api.requestWithoutPayload
                nonce
                apiKey
                apiSecret
                "https://stg.buda.com"
                endpoint
                (Http.expectString (RemoteData.fromResult >> GotResponse))
    in
    case method of
        GET ->
            requestWithoutPayloadForMethod "GET"

        POST ->
            requestWithPayloadForMethod "POST"

        PUT ->
            requestWithPayloadForMethod "PUT"

        PATCH ->
            requestWithPayloadForMethod "PATCH"

        DELETE ->
            requestWithPayloadForMethod "DELETE"


httpMethodFromString : String -> Maybe HttpMethod
httpMethodFromString asString =
    case asString of
        "GET" ->
            Just GET

        "POST" ->
            Just POST

        "PUT" ->
            Just PUT

        "PATCH" ->
            Just PATCH

        "DELETE" ->
            Just DELETE

        _ ->
            Nothing



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container mx-auto flex h-screen" ]
        [ leftHalf model, doRequestButton, rightHalf model ]


doRequestButton : Html Msg
doRequestButton =
    div [ class "w-24 flex justify-center items-center" ]
        [ div
            [ class "btn"
            , onClick DoRequestButtonPressed
            ]
            [ text ">>" ]
        ]


leftHalf : Model -> Html Msg
leftHalf model =
    div [ class "flex flex-col flex-1" ]
        [ div [ class "h-40 flex flex-col justify-center" ]
            [ textInput FormApiKeyUpdated "API Key" model.formApiKey
            , textInput FormApiSecretUpdated "API Secret" model.formApiSecret
            , textInput FormEndpointUpdated "Endpoint" model.formEndpoint
            , httpMethodSelect
            ]
        , payloadTitle
        , payloadTextArea
        ]


payloadTitle : Html Msg
payloadTitle =
    div [ class "text-2xl py-4" ] [ text "JSON Payload" ]


payloadTextArea : Html Msg
payloadTextArea =
    textarea
        [ class textAreaClass
        , onInput FormJsonPayloadUpdated
        ]
        []


textInput : (String -> Msg) -> String -> String -> Html Msg
textInput toMsg title value_ =
    div [ class "flex w-full border border-teal-300 border-solid rounded" ]
        [ div [ class "w-1/3 font-sans text-xl text-gray-800 text-right px-2 font-bold antialiased" ]
            [ text title
            ]
        , div [ class "w-2/3 border-l border-gray-500" ]
            [ input [ class "h-full w-full px-2 font-mono", onInput toMsg, value value_ ] [ text "hello" ]
            ]
        ]


httpMethodSelect : Html Msg
httpMethodSelect =
    div [ class "flex w-full" ]
        [ div [ class "w-1/3 font-sans text-m text-gray-800 text-right px-2 antialiased" ]
            [ text "Method" ]
        , select [ class "w-2/3", onInput FormHttpMethodSelected ]
            [ option [ value "GET" ] [ text "GET" ]
            , option [ value "POST" ] [ text "POST" ]
            , option [ value "PUT" ] [ text "PUT" ]
            , option [ value "PATCH" ] [ text "PATCH" ]
            , option [ value "DELETE" ] [ text "DELETE" ]
            ]
        ]


responseTitle : Html Msg
responseTitle =
    div [ class "text-2xl py-4" ] [ text "Response" ]


responseTextArea : WebData String -> Html Msg
responseTextArea response =
    let
        text_ =
            case response of
                RemoteData.Success successResponse ->
                    successResponse

                RemoteData.Loading ->
                    "Loading..."

                RemoteData.NotAsked ->
                    ""

                RemoteData.Failure err ->
                    "Error"
    in
    textarea
        [ class textAreaClass ]
        [ text text_ ]


textAreaClass : String
textAreaClass =
    "border p-4 rounded border-gray-500 text-gray-800 resize-none flex-1 mb-10"


rightHalf : Model -> Html Msg
rightHalf model =
    div [ class "flex flex-col flex-1" ]
        [ div
            [ class "h-40 flex justify-center" ]
            []
        , responseTitle
        , responseTextArea model.response
        ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always <| Time.every 1000 OneSecondPassed
        }
