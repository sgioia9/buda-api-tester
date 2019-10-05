module Main exposing (Model, Msg(..), init, main, update, view)

import Api.Api as Api
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, src, value)
import Html.Events exposing (onClick, onInput)
import Time



---- MODEL ----


type HttpMethod
    = GET
    | POST


type alias Model =
    { selectedHttpMethod : Maybe HttpMethod
    , formApiKey : String
    , formEndpoint : String
    , formMethod : String
    , formJsonPayload : String
    , currentTime : Time.Posix
    }


init : ( Model, Cmd Msg )
init =
    ( { selectedHttpMethod = Just GET
      , formApiKey = ""
      , formEndpoint = ""
      , formMethod = ""
      , formJsonPayload = ""
      , currentTime = 0 |> Time.millisToPosix
      }
    , Cmd.none
    )



---- UPDATE ----


type Msg
    = NoOp
    | FormHttpMethodSelected String
    | FormApiKeyUpdated String
    | FormEndpointUpdated String
    | FormJsonPayloadUpdated String
    | DoRequestButtonPressed
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

        FormEndpointUpdated updatedEndpoint ->
            ( { model | formEndpoint = updatedEndpoint }, Cmd.none )

        FormJsonPayloadUpdated updatedPayload ->
            ( { model | formJsonPayload = updatedPayload }, Cmd.none )

        DoRequestButtonPressed ->
            ( model, Cmd.none )

        OneSecondPassed currentTime ->
            ( { model | currentTime = currentTime }, Cmd.none )


httpMethodFromString : String -> Maybe HttpMethod
httpMethodFromString asString =
    case asString of
        "GET" ->
            Just GET

        "POST" ->
            Just POST

        _ ->
            Nothing



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "container mx-auto flex" ]
        [ leftHalf model, doRequestButton, rightHalf model ]


doRequestButton : Html Msg
doRequestButton =
    div [ class "w-24 flex justify-center items-center" ]
        [ div [ class "flex w-12 h-4 py-4 bg-teal-500 cursor-pointer hover:bg-teal-600 text-lg text-white items-center justify-center border rounded", onClick DoRequestButtonPressed ] [ text ">>" ]
        ]


leftHalf : Model -> Html Msg
leftHalf _ =
    div [ class "flex flex-col flex-1" ]
        [ div [ class "h-40 flex flex-col justify-center" ]
            [ textInput FormApiKeyUpdated "API Key"
            , textInput FormEndpointUpdated "Endpoint"
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
        [ class "border p-4 rounded border-gray-500 text-gray-800 resize-none h-64"
        , onInput FormJsonPayloadUpdated
        ]
        []


textInput : (String -> Msg) -> String -> Html Msg
textInput toMsg title =
    div [ class "flex w-full border border-teal-300 border-solid rounded" ]
        [ div [ class "w-1/3 font-sans text-xl text-gray-800 text-right px-2 font-bold antialiased" ]
            [ text title
            ]
        , div [ class "w-2/3 border-l border-gray-500" ]
            [ input [ class "h-full w-full px-2 font-mono", onInput toMsg ] [ text "hello" ]
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
            ]
        ]


responseTitle : Html Msg
responseTitle =
    div [ class "text-2xl py-4" ] [ text "Response" ]


responseTextArea : Html Msg
responseTextArea =
    textarea [ class "border p-4 rounded border-gray-500 text-gray-800 resize-none h-64" ] []


rightHalf : Model -> Html Msg
rightHalf _ =
    div [ class "flex flex-col flex-1" ]
        [ div [ class "h-40 flex justify-center" ] [], responseTitle, responseTextArea ]



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always <| Time.every 1000 OneSecondPassed
        }
