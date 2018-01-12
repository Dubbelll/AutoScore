module App exposing (..)

import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Jwt


main : Program Flags Model Msg
main =
    Html.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { config : Config, token : String, example : String }


type alias Flags =
    { version : String, baseURL : String }


type alias Config =
    { version : String, baseURL : String }


type alias JWT =
    { token : String }


type alias Example =
    { example : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { config = { version = flags.version, baseURL = flags.baseURL }, token = "", example = "" }, Cmd.none )


fullURL : Config -> String
fullURL config =
    config.baseURL ++ config.version


decodeJWT : Decode.Decoder JWT
decodeJWT =
    Decode.map JWT
        (Decode.field "token" Decode.string)


decodeExample : Decode.Decoder Example
decodeExample =
    Decode.map Example
        (Decode.field "example" Decode.string)



-- UPDATE


type Msg
    = Authenticate
    | NewToken (Result Http.Error JWT)
    | RetrieveExample
    | NewExample (Result Http.Error Example)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Authenticate ->
            ( model, authenticate model )

        NewToken (Ok newToken) ->
            ( { model | token = newToken.token }, Cmd.none )

        NewToken (Err _) ->
            ( model, Cmd.none )

        RetrieveExample ->
            ( model, retrieveExample model )

        NewExample (Ok newExample) ->
            ( { model | example = newExample.example }, Cmd.none )

        NewExample (Err _) ->
            ( model, Cmd.none )


authenticate : Model -> Cmd Msg
authenticate model =
    let
        url =
            fullURL model.config ++ "/authenticate"

        request =
            Http.post url Http.emptyBody decodeJWT
    in
    Http.send NewToken request


retrieveExample : Model -> Cmd Msg
retrieveExample model =
    let
        url =
            fullURL model.config ++ "/example/authorized"

        request =
            Jwt.createRequest "GET" model.token url Http.emptyBody decodeExample
    in
    Http.send NewExample request



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "GeneriekeFrontendSeed" ]
        , button [ onClick Authenticate ] [ text "Authenticate" ]
        , button [ onClick RetrieveExample ] [ text "Authorized retrieval" ]
        ]
