module App exposing (..)

import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode
import Json.Encode as Encode exposing (Value, encode, object, string)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { token : String, docs : String }


type alias JWT =
    { token : String }


type alias Config =
    { key : String, version : String, baseURL : String }


init : ( Model, Cmd Msg )
init =
    ( { token = "", docs = "" }, Cmd.none )


config : Config
config =
    { key = "be37fd3a-a070-4e44-815e-f430e1e5dfd2", version = "/v1", baseURL = "http://localhost:3000/api" }


fullURL : String
fullURL =
    config.baseURL ++ config.version


decodeJWT : Decode.Decoder JWT
decodeJWT =
    Decode.map JWT
        (Decode.field "token" Decode.string)


encodeAPIKey : String -> Encode.Value
encodeAPIKey key =
    Encode.object
        [ ( "api_key", Encode.string key )
        ]



-- UPDATE


type Msg
    = Authenticate
    | NewToken (Result Http.Error JWT)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Authenticate ->
            ( model, authenticate config.key )

        NewToken (Ok newToken) ->
            ( { model | token = newToken.token }, Cmd.none )

        NewToken (Err _) ->
            ( model, Cmd.none )


authenticate : String -> Cmd Msg
authenticate key =
    let
        url =
            fullURL ++ "/authenticate"

        body =
            encodeAPIKey key |> Http.jsonBody

        request =
            Http.post url body decodeJWT
    in
    Http.send NewToken request



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
        ]
