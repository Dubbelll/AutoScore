module App exposing (..)

import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode


main : Program Flags Model Msg
main =
    Html.programWithFlags { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { config : Config, token : String }


type alias Flags =
    { version : String, baseURL : String }


type alias JWT =
    { token : String }


type alias Config =
    { version : String, baseURL : String }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { config = { version = flags.version, baseURL = flags.baseURL }, token = "" }, Cmd.none )


fullURL : Config -> String
fullURL config =
    config.baseURL ++ config.version


decodeJWT : Decode.Decoder JWT
decodeJWT =
    Decode.map JWT
        (Decode.field "token" Decode.string)



-- UPDATE


type Msg
    = Authenticate
    | NewToken (Result Http.Error JWT)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Authenticate ->
            ( model, authenticate model )

        NewToken (Ok newToken) ->
            ( { model | token = newToken.token }, Cmd.none )

        NewToken (Err _) ->
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
