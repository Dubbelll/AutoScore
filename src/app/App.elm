module App exposing (..)

import Html exposing (Html, button, div, h1, text)
import Html.Events exposing (onClick)
import Navigation exposing (Location)
import Translation.App as Translation
import UrlParser exposing (..)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP


main : Program Flags Model Msg
main =
    Navigation.programWithFlags LocationChange { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type Route
    = RouteLanding
    | RouteShop
    | RouteInfo
    | RouteNotFound


type alias Model =
    { config : Config, route : Route, examples : List Example }


type alias Flags =
    { version : String, baseURL : String }


type alias Config =
    { version : String, baseURL : String }


type alias Example =
    { id : String, description : String, added_date_time : String }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        currentRoute =
            parseLocation location
    in
        ( { config = { version = flags.version, baseURL = flags.baseURL }, route = currentRoute, examples = [] }, Cmd.none )


fullURL : Config -> String
fullURL config =
    config.baseURL ++ config.version


decodeExample : JD.Decoder Example
decodeExample =
    JDP.decode Example
        |> JDP.required "id" JD.string
        |> JDP.required "description" JD.string
        |> JDP.required "added_date_time" JD.string


decodeExamples : JD.Decoder (List Example)
decodeExamples =
    JD.list decodeExample



-- DATA
-- ROUTING


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map RouteLanding top
        , map RouteShop (s Translation.locationShop)
        , map RouteInfo (s Translation.locationInfo)
        ]


parseLocation : Location -> Route
parseLocation location =
    case parseHash matchers location of
        Just route ->
            route

        Nothing ->
            RouteNotFound



-- UPDATE


type Msg
    = LocationChange Location
    | RetrieveExamples
    | NewExamples (Result Http.Error (List Example))


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        LocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )

        RetrieveExamples ->
            ( model, retrieveExamples model )

        NewExamples (Ok newExamples) ->
            ( { model | examples = newExamples }, Cmd.none )

        NewExamples (Err _) ->
            ( model, Cmd.none )


retrieveExamples : Model -> Cmd Msg
retrieveExamples model =
    let
        url =
            fullURL model.config ++ "/examples"

        request =
            Http.get url decodeExamples
    in
        Http.send NewExamples request



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "CarCrashMystery" ]
        , button [ onClick RetrieveExamples ] [ text "Retrieve example" ]
        ]
