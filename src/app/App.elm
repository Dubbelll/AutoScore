module App exposing (..)

import Html exposing (Html, h1, text)
import Message exposing (..)


main : Program Never Model Msg
main =
    Html.program { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { display : String }


init : ( Model, Cmd Msg )
init =
    ( { display = "Hello, Elm!" }, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Request ->
            ( { model | display = "Requesting..." }, Cmd.none )

        Response ->
            ( { model | display = "Responding..." }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    h1 [] [ text model.display ]
