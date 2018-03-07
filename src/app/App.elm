module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation exposing (Location)
import UrlParser as UP
import Json.Decode as JD
import Ports as PT
import Array exposing (Array)


main : Program Flags Model Msg
main =
    Navigation.programWithFlags LocationChange { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { config : Config
    , route : Route
    , location : Location
    , imageId : String
    , imageSource : Maybe Image
    , amountStones : Int
    , boardSizes : List BoardSize
    , boardSize : BoardSize
    , board : Board
    }


type alias Flags =
    { version : String, baseURL : String }


type alias Config =
    { version : String, baseURL : String }


type alias Image =
    { dataArray : Array Int, dataBase64 : String, width : Int, height : Int }


type StoneColor
    = White
    | Black


type alias Stone =
    { x : Int, y : Int, liberties : Int, color : StoneColor }


type alias Prisoner =
    StoneColor


type BoardSize
    = Nineteen
    | Thirteen
    | Nine


type alias Board =
    Array Stone


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
            , amountStones = 284
            , boardSizes = [ Nineteen, Thirteen, Nine ]
            , boardSize = Nineteen
            , board = Array.empty
            }
    in
        ( model, Cmd.none )



-- MODEL HELPERS


boardSizeToString : BoardSize -> String
boardSizeToString boardSize =
    case boardSize of
        Nineteen ->
            "19x19"

        Thirteen ->
            "13x13"

        Nine ->
            "9x9"


boardSizeToRange : BoardSize -> List Int
boardSizeToRange boardSize =
    case boardSize of
        Nineteen ->
            List.range 1 19

        Thirteen ->
            List.range 1 13

        Nine ->
            List.range 1 9


stringToBoardSize : String -> BoardSize
stringToBoardSize string =
    case string of
        "19x19" ->
            Nineteen

        "13x13" ->
            Thirteen

        "9x9" ->
            Nine

        _ ->
            Nineteen



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
    | NewAmountStones String
    | NewBoardSize String


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
                    { dataArray = imageData.dataArray
                    , dataBase64 = imageData.dataBase64
                    , width = imageData.width
                    , height = imageData.height
                    }
            in
                ( { model | imageSource = Just newImage }, Cmd.none )

        NewAmountStones amount ->
            let
                newAmount =
                    Result.withDefault 0 (String.toInt amount)
            in
                ( { model | amountStones = newAmount }, Cmd.none )

        NewBoardSize boardSize ->
            let
                newBoardSize =
                    stringToBoardSize boardSize
            in
                ( { model | boardSize = newBoardSize }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    PT.processImage ImageProcessed



-- VIEW


view : Model -> Html Msg
view model =
    div [ classList [ ( "container-app", True ) ] ]
        [ viewImageSource model
        , viewBoard model
        ]


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
            , input [ type_ "number", onInput NewAmountStones, value (toString model.amountStones) ] []
            , select [ on "change" (JD.map NewBoardSize targetValue) ]
                (List.map (viewOptionBoardSize model) model.boardSizes)
            , imagePreview
            ]


viewImage : Image -> Html Msg
viewImage image =
    img [ src image.dataBase64 ] []


viewOptionBoardSize : Model -> BoardSize -> Html Msg
viewOptionBoardSize model boardSize =
    option [ value (boardSizeToString boardSize), selected (model.boardSize == boardSize) ] [ text (boardSizeToString boardSize) ]


viewBoard : Model -> Html Msg
viewBoard model =
    div
        [ classList
            [ ( "container-board", True )
            , ( "container-board--19x19", model.boardSize == Nineteen )
            , ( "container-board--13x13", model.boardSize == Thirteen )
            , ( "container-board--9x9", model.boardSize == Nine )
            ]
        ]
        (List.map (viewBoardRow model) (boardSizeToRange model.boardSize))


viewBoardRow : Model -> Int -> Html Msg
viewBoardRow model y =
    div [ classList [ ( "board-row", True ) ] ]
        (List.map (viewBoardColumn model) (boardSizeToRange model.boardSize))


viewBoardColumn : Model -> Int -> Html Msg
viewBoardColumn model x =
    div
        [ classList
            [ ( "board-column", True )
            , ( "board-column--19x19", model.boardSize == Nineteen )
            , ( "board-column--13x13", model.boardSize == Thirteen )
            , ( "board-column--9x9", model.boardSize == Nine )
            ]
        ]
        [ span [ classList [ ( "board-point", True ) ] ] []
        , span [ classList [ ( "board-connection", True ), ( "board-connection--right", True ) ] ] []
        , span [ classList [ ( "board-connection", True ), ( "board-connection--down", True ) ] ] []
        ]
