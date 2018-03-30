module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation exposing (Location)
import UrlParser as UP
import Json.Decode as JD
import Ports as PT
import Array exposing (Array)
import Svg exposing (svg, use)
import Svg.Attributes as SA exposing (class, xlinkHref)


main : Program Flags Model Msg
main =
    Navigation.programWithFlags ChangeLocation { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { config : Config
    , route : Route
    , location : Location
    , isLoading : Bool
    , loadingMessage : Maybe String
    , amountStones : Int
    , boardSizes : List BoardSize
    , boardSize : BoardSize
    , isVideoPlaying : Bool
    , isInputSuccessful : Bool
    , isCroppingSuccessful : Bool
    , isPickingBlackSuccessful : Bool
    , isPickingWhiteSuccessful : Bool
    , isProcessingSuccessful : Bool
    , probabilities : Array PT.Probability
    , stars19x19 : List Star
    , stars13x13 : List Star
    , stars9x9 : List Star
    , board : Board
    }


type alias Flags =
    { version : String, baseURL : String }


type alias Config =
    { version : String, baseURL : String }


type TextDirection
    = ToLeft
    | ToRight


type CropShape
    = Rectangle
    | Circle


type alias Star =
    { x : Int, y : Int }


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
            , isLoading = False
            , loadingMessage = Nothing
            , amountStones = 284
            , boardSizes = [ Nineteen, Thirteen, Nine ]
            , boardSize = Nineteen
            , isVideoPlaying = False
            , isInputSuccessful = False
            , isCroppingSuccessful = False
            , isPickingBlackSuccessful = False
            , isPickingWhiteSuccessful = False
            , isProcessingSuccessful = False
            , probabilities = Array.empty
            , stars19x19 = stars19x19
            , stars13x13 = stars13x13
            , stars9x9 = stars9x9
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


stars19x19 : List Star
stars19x19 =
    [ Star 4 4
    , Star 10 4
    , Star 16 4
    , Star 4 10
    , Star 10 10
    , Star 16 10
    , Star 4 16
    , Star 10 16
    , Star 16 16
    ]


stars13x13 : List Star
stars13x13 =
    [ Star 4 4
    , Star 10 4
    , Star 7 7
    , Star 4 10
    , Star 10 10
    ]


stars9x9 : List Star
stars9x9 =
    [ Star 3 3
    , Star 7 3
    , Star 5 5
    , Star 3 7
    , Star 7 7
    ]


isStar : Model -> Int -> Int -> Bool
isStar model x y =
    case model.boardSize of
        Nineteen ->
            List.any (\z -> z == (Star x y)) model.stars19x19

        Thirteen ->
            List.any (\z -> z == (Star x y)) model.stars13x13

        Nine ->
            List.any (\z -> z == (Star x y)) model.stars9x9



-- ROUTING


type Route
    = RouteLanding
    | RoutePhoto
    | RouteCrop
    | RouteBlack
    | RouteWhite
    | RouteProcessing
    | RouteScore
    | RouteNotFound


matchers : UP.Parser (Route -> a) a
matchers =
    UP.oneOf
        [ UP.map RouteLanding UP.top
        , UP.map RoutePhoto (UP.s "photo")
        , UP.map RouteCrop (UP.s "crop")
        , UP.map RouteBlack (UP.s "black")
        , UP.map RouteWhite (UP.s "white")
        , UP.map RouteProcessing (UP.s "processing")
        , UP.map RouteScore (UP.s "score")
        ]


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
    | ChangeLocation Location
    | StartCamera
    | NewFile
    | NewAmountStones String
    | NewBoardSize String
    | CameraStarted Bool
    | CameraStopped Bool
    | TakePhoto
    | InputSuccessful Bool
    | CropPhoto
    | CroppingSuccessful Bool
    | PickBlack
    | PickingBlackSuccessful Bool
    | PickWhite
    | PickingWhiteSuccessful Bool
    | ProcessingSuccessful (Array PT.Probability)


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ChangePath path ->
            ( model, Navigation.newUrl path )

        ChangeLocation location ->
            let
                newRoute =
                    parseLocation location

                newModel =
                    { model | route = newRoute, location = location }

                commandOldRoute =
                    if model.route == RoutePhoto && model.isVideoPlaying then
                        PT.stopCamera True
                    else
                        Cmd.none

                commandNewRoute =
                    case newRoute of
                        RouteCrop ->
                            PT.startCropping True

                        RouteBlack ->
                            PT.startPickingBlack True

                        RouteWhite ->
                            PT.startPickingWhite True

                        RouteProcessing ->
                            PT.startProcessing True

                        _ ->
                            Cmd.none

                newCommands =
                    Cmd.batch [ commandOldRoute, commandNewRoute ]
            in
                ( newModel, newCommands )

        StartCamera ->
            ( { model | isLoading = True, loadingMessage = Just "Starting camera" }, PT.startCamera True )

        NewFile ->
            ( model, PT.useFile "file-input" )

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

        CameraStarted isPlaying ->
            ( { model | isVideoPlaying = isPlaying, isLoading = False, loadingMessage = Nothing }, Cmd.none )

        CameraStopped isPlaying ->
            ( { model | isVideoPlaying = isPlaying }, Cmd.none )

        TakePhoto ->
            ( model, PT.takePhoto True )

        InputSuccessful isSuccessful ->
            ( { model | isInputSuccessful = isSuccessful }, Navigation.newUrl "#crop" )

        CropPhoto ->
            ( model, PT.cropPhoto True )

        CroppingSuccessful isSuccessful ->
            ( { model | isCroppingSuccessful = isSuccessful }, Navigation.newUrl "#black" )

        PickBlack ->
            ( model, PT.pickBlack True )

        PickingBlackSuccessful isSuccessful ->
            ( { model | isPickingBlackSuccessful = isSuccessful }, Navigation.newUrl "#white" )

        PickWhite ->
            ( model, PT.pickWhite True )

        PickingWhiteSuccessful isSuccessful ->
            ( { model | isPickingWhiteSuccessful = isSuccessful }, Navigation.newUrl "#processing" )

        ProcessingSuccessful probabilities ->
            ( { model | isProcessingSuccessful = True, probabilities = probabilities }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PT.cameraStarted CameraStarted
        , PT.cameraStopped CameraStopped
        , PT.inputSuccessful InputSuccessful
        , PT.croppingSuccessful CroppingSuccessful
        , PT.pickingBlackSuccessful PickingBlackSuccessful
        , PT.pickingWhiteSuccessful PickingWhiteSuccessful
        , PT.processingSuccessful ProcessingSuccessful
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ classList [ ( "container-app", True ) ] ]
        [ page model
        , viewCanvas "canvas-input" (model.isInputSuccessful && model.route == RouteCrop)
        , viewCanvas "canvas-color-black" (model.isCroppingSuccessful && model.route == RouteBlack)
        , viewCanvas "canvas-color-white" (model.isPickingBlackSuccessful && model.route == RouteWhite)
        , viewCanvas "canvas-output" (model.isPickingWhiteSuccessful && model.route == RouteProcessing)
        , viewCanvas "canvas-temporary" False
        , viewCropFrame "crop-input" Rectangle (model.isInputSuccessful && model.route == RouteCrop)
        , viewCropFrame "crop-color-black" Circle (model.isCroppingSuccessful && model.route == RouteBlack)
        , viewCropFrame "crop-color-white" Circle (model.isPickingBlackSuccessful && model.route == RouteWhite)
        , video
            [ id "video"
            , classList [ ( "video", True ), ( "video--visible", model.isVideoPlaying ) ]
            , onClick TakePhoto
            ]
            []
        , div [ classList [ ( "loading", True ), ( "loading--visible", model.isLoading ) ] ]
            [ div [ classList [ ( "loading-animation", True ) ] ]
                [ span [ classList [ ( "stone", True ), ( "stone--black", True ) ] ] []
                , span [ classList [ ( "stone", True ), ( "stone--white", True ) ] ] []
                ]
            , div [ classList [ ( "loading-message", True ) ] ]
                [ text (Maybe.withDefault "Loading" model.loadingMessage)
                ]
            ]
        ]


page : Model -> Html Msg
page model =
    case model.route of
        RouteLanding ->
            viewLanding model

        RoutePhoto ->
            viewPhoto model

        RouteCrop ->
            viewCrop model

        RouteBlack ->
            viewBlack model

        RouteWhite ->
            viewWhite model

        RouteProcessing ->
            viewProcessing model

        RouteScore ->
            viewScore model

        RouteNotFound ->
            div [] []



-- VIEW GENERAL


viewCanvas : String -> Bool -> Html Msg
viewCanvas identifier isVisible =
    canvas
        [ id identifier
        , classList [ ( "canvas", True ), ( "canvas--visible", isVisible ) ]
        ]
        []


viewCropFrame : String -> CropShape -> Bool -> Html Msg
viewCropFrame identifier shape isVisible =
    let
        content =
            case shape of
                Rectangle ->
                    [ div
                        [ id identifier
                        , classList
                            [ ( "crop-frame", True )
                            , ( "crop-frame--rectangle", True )
                            ]
                        ]
                        [ img [ id (identifier ++ "-image"), classList [ ( "crop-frame--image", True ) ] ] []
                        , div [ id (identifier ++ "-move"), classList [ ( "crop-frame--move", True ) ] ] []
                        , div [ id (identifier ++ "-resize"), classList [ ( "crop-frame--resize", True ) ] ] []
                        ]
                    ]

                Circle ->
                    [ div
                        [ id identifier
                        , classList
                            [ ( "crop-frame", True )
                            , ( "crop-frame--circle", True )
                            ]
                        ]
                        [ img [ id (identifier ++ "-image"), classList [ ( "crop-frame--image", True ) ] ] []
                        , div [ id (identifier ++ "-move"), classList [ ( "crop-frame--move", True ) ] ] []
                        ]
                    , div [ id (identifier ++ "-preview"), classList [ ( "crop-frame-preview", True ) ] ]
                        [ img [ id (identifier ++ "-preview-image"), classList [ ( "crop-frame-preview--image", True ) ] ] [] ]
                    , input [ id (identifier ++ "-slider"), classList [ ( "crop-frame-slider", True ) ], type_ "range" ] []
                    ]
    in
        div [ classList [ ( "container-crop-frame", True ), ( "container-crop-frame--visible", isVisible ) ] ]
            [ div
                [ id (identifier ++ "-fader")
                , classList
                    [ ( "crop-frame-fader", True ) ]
                ]
                []
            , div
                [ id (identifier ++ "-boundary")
                , classList
                    [ ( "crop-frame-boundary", True ) ]
                ]
                content
            ]


viewIcon : String -> List ( String, Bool ) -> Html Msg
viewIcon name classesExtra =
    let
        classes =
            [ ( "icon", True ) ] ++ classesExtra
    in
        svg [ SA.class (classString classes) ]
            [ use [ xlinkHref ("#" ++ name) ] [] ]


viewIconText : String -> List ( String, Bool ) -> String -> TextDirection -> Bool -> Html Msg
viewIconText name classesExtra value direction isClickable =
    let
        classes =
            [ ( "container-icon-text", True ), ( "link-fake", isClickable ) ] ++ classesExtra

        content =
            case direction of
                ToLeft ->
                    [ span [ classList [ ( "text-icon", True ) ] ] [ text value ]
                    , viewIcon name [ ( "icon--big", True ) ]
                    ]

                ToRight ->
                    [ viewIcon name [ ( "icon--big", True ) ]
                    , span [ classList [ ( "text-icon", True ) ] ] [ text value ]
                    ]
    in
        span [ classList classes ]
            content


viewIconTextLink : String -> List ( String, Bool ) -> String -> TextDirection -> Msg -> Html Msg
viewIconTextLink name classesExtra value direction msg =
    let
        classes =
            [ ( "container-icon-text-link", True ), ( "link-fake", True ) ] ++ classesExtra

        content =
            case direction of
                ToLeft ->
                    [ span [ classList [ ( "text-icon", True ) ] ] [ text value ]
                    , viewIcon name [ ( "icon--big", True ) ]
                    ]

                ToRight ->
                    [ viewIcon name [ ( "icon--big", True ) ]
                    , span [ classList [ ( "text-icon", True ) ] ] [ text value ]
                    ]
    in
        span [ classList classes, onClick msg ]
            content


buttonClose : Bool -> Html Msg
buttonClose isOverlay =
    viewIconTextLink "close" [ ( "close", True ), ( "close--overlay", isOverlay ) ] "Close" ToLeft (ChangePath "#")



-- VIEW CONTENT


viewLanding : Model -> Html Msg
viewLanding model =
    div [ classList [ ( "container-landing", True ) ] ]
        [ ul []
            [ li [ classList [ ( "container-link", True ), ( "list-item", True ) ] ]
                [ viewIconTextLink "camera" [] "Photo" ToRight (ChangePath "#photo") ]
            , li [ classList [ ( "container-input", True ), ( "list-item", True ) ] ]
                [ label [ for "file-input" ]
                    [ viewIconText "file" [] "File" ToRight True
                    , input [ id "file-input", type_ "file", on "change" (JD.succeed NewFile) ] []
                    ]
                ]
            , li [ classList [ ( "container-input", True ), ( "list-item", True ) ] ]
                [ label [ for "stones-amount" ]
                    [ viewIconText "stones" [] "Number of stones" ToRight False
                    , input
                        [ id "stones-amount"
                        , classList [ ( "input", True ) ]
                        , type_ "number"
                        , onInput NewAmountStones
                        , value (toString model.amountStones)
                        ]
                        []
                    ]
                ]
            , li [ classList [ ( "container-select", True ), ( "list-item", True ) ] ]
                [ viewIconText "grid" [] "Board size" ToRight False
                , select [ classList [ ( "input", True ) ], on "change" (JD.map NewBoardSize targetValue) ]
                    (List.map (viewOptionBoardSize model) model.boardSizes)
                ]
            ]
        ]


viewOptionBoardSize : Model -> BoardSize -> Html Msg
viewOptionBoardSize model boardSize =
    option [ value (boardSizeToString boardSize), selected (model.boardSize == boardSize) ] [ text (boardSizeToString boardSize) ]


viewPhoto : Model -> Html Msg
viewPhoto model =
    let
        overlay =
            case model.isVideoPlaying of
                True ->
                    [ buttonClose True
                    , viewIconText
                        "info"
                        [ ( "camera", True ), ( "camera--overlay", True ), ( "camera--info", True ) ]
                        "Tap/click anywhere to take a photo"
                        ToRight
                        False
                    ]

                False ->
                    [ buttonClose False
                    , viewIconTextLink
                        "camera"
                        [ ( "camera", True ), ( "camera--start", True ) ]
                        "Start camera"
                        ToRight
                        StartCamera
                    ]
    in
        div [ classList [ ( "container-photo", True ) ] ]
            overlay


viewCrop : Model -> Html Msg
viewCrop model =
    let
        overlay =
            case model.isInputSuccessful of
                True ->
                    [ buttonClose True
                    , viewIconText
                        "info"
                        [ ( "crop", True ), ( "crop--overlay", True ), ( "crop--info", True ) ]
                        "Crop the image so only the board remains"
                        ToRight
                        False
                    , viewIconTextLink
                        "crop"
                        [ ( "crop", True ), ( "crop--overlay", True ), ( "crop--start", True ) ]
                        "Crop"
                        ToRight
                        CropPhoto
                    ]

                False ->
                    [ buttonClose False
                    , viewIconText
                        "info"
                        [ ( "crop", True ) ]
                        "Nothing to crop"
                        ToRight
                        False
                    ]
    in
        div
            [ classList [ ( "container-crop", True ) ] ]
            overlay


viewBlack : Model -> Html Msg
viewBlack model =
    let
        overlay =
            case model.isCroppingSuccessful of
                True ->
                    [ buttonClose True
                    , viewIconText
                        "info"
                        [ ( "color", True ), ( "color--overlay", True ), ( "color--info--black", True ) ]
                        "Select the lightest black stone"
                        ToRight
                        False
                    , viewIconTextLink
                        "color"
                        [ ( "color", True ), ( "color--overlay", True ), ( "color--start", True ) ]
                        "Pick"
                        ToRight
                        PickBlack
                    ]

                False ->
                    [ buttonClose False
                    , viewIconText
                        "info"
                        [ ( "color", True ) ]
                        "Nothing to pick"
                        ToRight
                        False
                    ]
    in
        div
            [ classList [ ( "container-color-black", True ) ] ]
            overlay


viewWhite : Model -> Html Msg
viewWhite model =
    let
        overlay =
            case model.isPickingBlackSuccessful of
                True ->
                    [ buttonClose True
                    , viewIconText
                        "info"
                        [ ( "color", True ), ( "color--overlay", True ), ( "color--info--white", True ) ]
                        "Select the darkest white stone"
                        ToRight
                        False
                    , viewIconTextLink
                        "color"
                        [ ( "color", True ), ( "color--overlay", True ), ( "color--start", True ) ]
                        "Pick"
                        ToRight
                        PickWhite
                    ]

                False ->
                    [ buttonClose False
                    , viewIconText
                        "info"
                        [ ( "color", True ) ]
                        "Nothing to pick"
                        ToRight
                        False
                    ]
    in
        div
            [ classList [ ( "container-color-white", True ) ] ]
            overlay


viewProcessing : Model -> Html Msg
viewProcessing model =
    let
        overlay =
            case model.isCroppingSuccessful of
                True ->
                    [ buttonClose True
                    , viewIconText
                        "info"
                        [ ( "score", True ), ( "score--overlay", True ), ( "score--info", True ) ]
                        "This is how I detected the stones on the board. You can make corrections in the next step"
                        ToRight
                        False
                    , viewIconTextLink
                        "score"
                        [ ( "score", True ), ( "score--overlay", True ), ( "score--start", True ) ]
                        "Score"
                        ToRight
                        (ChangePath "#score")
                    ]

                False ->
                    [ buttonClose False
                    , viewIconText
                        "info"
                        [ ( "crop", True ) ]
                        "Nothing to process"
                        ToRight
                        False
                    ]
    in
        div [ classList [ ( "container-processing", True ) ] ]
            overlay


viewScore : Model -> Html Msg
viewScore model =
    div
        [ classList [ ( "container-score", True ) ] ]
        [ buttonClose True
        , viewBoard model
        , div
            [ classList
                [ ( "container-board-surface", True )
                , ( "container-board-surface--19x19", model.boardSize == Nineteen )
                , ( "container-board-surface--13x13", model.boardSize == Thirteen )
                , ( "container-board-surface--13x13", model.boardSize == Nine )
                ]
            ]
            []
        ]


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
        (List.map (viewBoardColumn model y) (boardSizeToRange model.boardSize))


viewBoardColumn : Model -> Int -> Int -> Html Msg
viewBoardColumn model y x =
    let
        probability =
            Array.get ((x * y) - 1) model.probabilities
                |> Maybe.withDefault (PT.Probability -1 -1 -1)

        isBlack =
            probability.probabilityBlack > 0

        isWhite =
            probability.probabilityWhite > 0

        isConflict =
            probability.probabilityBlack > 0 && probability.probabilityWhite > 0
    in
        div
            [ classList
                [ ( "board-column", True )
                , ( "board-column--19x19", model.boardSize == Nineteen )
                , ( "board-column--13x13", model.boardSize == Thirteen )
                , ( "board-column--9x9", model.boardSize == Nine )
                ]
            ]
            [ span
                [ classList
                    [ ( "board-stone", True )
                    , ( "board-stone--black", isBlack )
                    , ( "board-stone--white", isWhite )
                    , ( "board-stone--conflict", isConflict )
                    ]
                ]
                []
            , span [ classList [ ( "board-point", True ), ( "board-point--star", isStar model x y ) ] ] []
            , span [ classList [ ( "board-connection", True ), ( "board-connection--right", True ) ] ] []
            , span [ classList [ ( "board-connection", True ), ( "board-connection--down", True ) ] ] []
            ]



-- VIEW HELPERS


classToString : ( String, Bool ) -> String
classToString tuple =
    case Tuple.second tuple of
        True ->
            Tuple.first tuple

        False ->
            ""


classString : List ( String, Bool ) -> String
classString classes =
    let
        classList =
            List.map classToString classes
    in
        String.join " " classList


intToPixels : Int -> String
intToPixels value =
    toString value ++ "px"
