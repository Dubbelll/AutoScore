module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation exposing (Location)
import Translation.App as Translation
import UrlParser as UP
import Http
import Json.Decode as JD
import Date exposing (Date)
import Time exposing (Time)
import Svg exposing (svg, use)
import Svg.Attributes as SA exposing (class, xlinkHref)
import Dict exposing (Dict)
import Ports as PT


main : Program Flags Model Msg
main =
    Navigation.programWithFlags LocationChange { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { config : Config, route : Route, location : Location, imageId : String, imageSource : Maybe Image }


type alias Flags =
    { version : String, baseURL : String }


type alias Config =
    { version : String, baseURL : String }


type alias Image =
    { id : String, dataArray : List Int, dataBase64 : String, width : Int, height : Int }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        currentRoute =
            parseLocation location

        config =
            { version = flags.version, baseURL = flags.baseURL }

        model =
            { config = config
            , route = currentRoute
            , location = location
            , imageId = "image-source"
            , imageSource = Nothing
            }
    in
        ( model, Cmd.none )



-- ROUTING


type Route
    = RouteLanding
    | RouteNotFound


matchers : UP.Parser (Route -> a) a
matchers =
    UP.oneOf
        [ UP.map RouteLanding UP.top ]


parseLocation : Location -> Route
parseLocation location =
    case UP.parseHash matchers location of
        Just route ->
            route

        Nothing ->
            RouteNotFound



-- UPDATE


type Msg
    = ChangePath String
    | LocationChange Location
    | ImageSelected
    | ImageProcessed PT.ImageData


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ChangePath path ->
            ( model, Navigation.newUrl path )

        LocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | route = newRoute, location = location }, Cmd.none )

        ImageSelected ->
            ( model, PT.imageSelected model.imageId )

        ImageProcessed imageData ->
            let
                newImage =
                    { id = model.imageId
                    , dataArray = imageData.dataArray
                    , dataBase64 = imageData.dataBase64
                    , width = imageData.width
                    , height = imageData.height
                    }
            in
                ( { model | imageSource = Just newImage }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    PT.processImage ImageProcessed



-- VIEW


view : Model -> Html Msg
view model =
    div [ classList [ ( "container-app", True ) ] ]
        [ viewImageSource model ]


viewImageSource : Model -> Html Msg
viewImageSource model =
    let
        imagePreview =
            case model.imageSource of
                Just image ->
                    viewImage image

                Nothing ->
                    text ""
    in
        div [ classList [ ( "container-image", True ) ] ]
            [ input [ id model.imageId, type_ "file", on "change" (JD.succeed ImageSelected) ] []
            , imagePreview
            ]


viewImage : Image -> Html Msg
viewImage image =
    img [ src image.dataBase64 ] []
