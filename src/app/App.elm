module App exposing (..)

import Html exposing (Html, button, div, h1, text, img, input)
import Html.Attributes exposing (src, title, class, id, type_)
import Html.Events exposing (on, onClick)
import Navigation exposing (Location)
import Translation.App as Translation
import UrlParser exposing (..)
import Http
import Json.Decode as JD
import Json.Decode.Pipeline as JDP
import Ports as PT exposing (..)
import Dict exposing (Dict)


main : Program Flags Model Msg
main =
    Navigation.programWithFlags LocationChange { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type Route
    = RouteLanding
    | RouteNotFound


type alias Model =
    { config : Config, route : Route, examples : List Example, files : Files }


type alias Flags =
    { version : String, baseURL : String }


type alias Config =
    { version : String, baseURL : String }


type alias File =
    { name : String, content : String }


type alias Files =
    Dict String File


type alias Example =
    { id : Int, description : String, addedDateTime : String }


type alias Contact =
    { id : Int, smartphoneId : Int, name : String }


init : Flags -> Location -> ( Model, Cmd Msg )
init flags location =
    let
        currentRoute =
            parseLocation location
    in
        ( { config = { version = flags.version, baseURL = flags.baseURL }, route = currentRoute, examples = [], files = Dict.empty }, Cmd.none )



-- DECODERS


decodeExample : JD.Decoder Example
decodeExample =
    JDP.decode Example
        |> JDP.required "id" JD.int
        |> JDP.required "description" JD.string
        |> JDP.required "added_date_time" JD.string


decodeExamples : JD.Decoder (List Example)
decodeExamples =
    JD.list decodeExample



-- ROUTING


matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ map RouteLanding top
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
    | FileSelected FileSubscriptionPort
    | FileRead FileDataPort
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

        FileSelected subscription ->
            ( model, PT.fileSelected subscription )

        FileRead file ->
            let
                newFile =
                    { name = file.name, content = file.content }

                newKey =
                    file.key
            in
                ( { model | files = Dict.insert newKey newFile model.files }, Cmd.none )

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
            model.config.baseURL ++ "/examples"

        request =
            Http.get url decodeExamples
    in
        Http.send NewExamples request



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    PT.fileContentRead FileRead



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text "CarCrashMystery" ]
        , button [ onClick RetrieveExamples ] [ text "Retrieve example" ]
        , viewFileWithPreview model { key = "appIcon", id = "file-app-icon" }
        , viewFileWithPreview model { key = "walletPicture", id = "file-wallet-picture" }
        ]


viewFileWithPreview : Model -> FileSubscriptionPort -> Html Msg
viewFileWithPreview model subscription =
    let
        filePreview =
            case Dict.get subscription.key model.files of
                Just file ->
                    viewFileImage file

                Nothing ->
                    text ""
    in
        div [ class "file-upload-container" ]
            [ input [ id subscription.id, type_ "file", on "change" (JD.succeed (FileSelected subscription)) ] []
            , filePreview
            ]


viewFileImage : File -> Html Msg
viewFileImage file =
    img [ src file.content, title file.name ] []
