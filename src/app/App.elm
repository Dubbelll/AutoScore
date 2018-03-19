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
    , crop : PT.Crop
    , detections : Array PT.Detection
    , isProcessingSuccessful : Bool
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
            , crop = PT.Crop 0 0 0 0
            , detections = Array.empty
            , isProcessingSuccessful = False
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


median : List Int -> Float
median numbers =
    let
        isEven =
            (List.length numbers) % 2 == 0

        sizeHalf =
            (List.length numbers) // 2

        firstHalf =
            List.take sizeHalf numbers

        secondHalf =
            List.drop sizeHalf numbers
    in
        case isEven of
            True ->
                let
                    lastOfFirst =
                        List.drop (sizeHalf - 1) firstHalf
                            |> List.head
                            |> Maybe.withDefault 0
                            |> toFloat

                    firstOfLast =
                        List.take 1 secondHalf
                            |> List.head
                            |> Maybe.withDefault 0
                            |> toFloat
                in
                    (lastOfFirst + firstOfLast) / 2

            False ->
                List.take 1 secondHalf
                    |> List.head
                    |> Maybe.withDefault 0
                    |> toFloat


interquartileRange : List Int -> Float
interquartileRange numbers =
    let
        isEven =
            (List.length numbers) % 2 == 0

        sizeHalf =
            (List.length numbers) // 2

        firstHalf =
            List.take sizeHalf numbers

        secondHalf =
            case isEven of
                True ->
                    List.drop sizeHalf numbers

                False ->
                    List.drop (sizeHalf + 1) numbers

        q1 =
            median firstHalf

        q3 =
            median secondHalf
    in
        q3 - q1


furthestFrom : Float -> Int -> Int -> Int
furthestFrom from a b =
    case abs ((toFloat a) - from) > abs ((toFloat b) - from) of
        True ->
            a

        False ->
            b


removeOutliers : List Int -> List Int
removeOutliers numbers =
    case List.head numbers of
        Just first ->
            let
                average =
                    (toFloat (List.sum numbers)) / (toFloat (List.length numbers))

                furthestFromAverage =
                    List.foldl (furthestFrom average) first numbers

                maxDistance =
                    (interquartileRange numbers) * 1.5
            in
                case (abs ((toFloat furthestFromAverage) - average)) > maxDistance of
                    True ->
                        removeOutliers (List.filter (\x -> x /= furthestFromAverage) numbers)

                    False ->
                        numbers

        Nothing ->
            numbers


averageStoneWidth : List PT.Detection -> Float
averageStoneWidth detections =
    let
        sortedWidths =
            List.map .width detections
                |> List.sort

        singleStoneWidths =
            removeOutliers sortedWidths
    in
        (toFloat (List.sum singleStoneWidths)) / (toFloat (List.length singleStoneWidths))


averageStoneHeight : List PT.Detection -> Float
averageStoneHeight detections =
    let
        sortedHeights =
            List.map .height detections
                |> List.sort

        singleStoneHeights =
            removeOutliers sortedHeights
    in
        (toFloat (List.sum singleStoneHeights)) / (toFloat (List.length singleStoneHeights))


xEdge : List PT.Detection -> (List Int -> Maybe Int) -> Int
xEdge detections minOrMax =
    List.map .x detections
        |> minOrMax
        |> Maybe.withDefault 0


yEdge : List PT.Detection -> (List Int -> Maybe Int) -> Int
yEdge detections minOrMax =
    List.map .y detections
        |> minOrMax
        |> Maybe.withDefault 0


detectionToStone : PT.Detection -> Int -> Int -> Int -> Int -> Array Stone
detectionToStone detection xMin xMax yMin yMax =
    Array.empty


detectionsToStones : Array PT.Detection -> Board
detectionsToStones detectionsArray =
    let
        detections =
            Array.toList detectionsArray

        averageWidth =
            averageStoneWidth detections

        averageHeight =
            averageStoneHeight detections

        xMin =
            xEdge detections List.minimum

        xMax =
            xEdge detections List.maximum

        yMin =
            yEdge detections List.minimum

        yMax =
            yEdge detections List.maximum
    in
        Array.empty



-- ROUTING


type Route
    = RouteLanding
    | RoutePhoto
    | RouteCrop
    | RouteProcessing
    | RouteScore
    | RouteNotFound


matchers : UP.Parser (Route -> a) a
matchers =
    UP.oneOf
        [ UP.map RouteLanding UP.top
        , UP.map RoutePhoto (UP.s "photo")
        , UP.map RouteCrop (UP.s "crop")
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
    | CroppingSuccessful PT.Crop
    | StoneDetected PT.Detection
    | ProcessingSuccessful Bool
    | DetectionsToStones


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
                    if newRoute == RouteProcessing then
                        { model | route = newRoute, location = location, detections = Array.empty }
                    else
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

        CroppingSuccessful newCrop ->
            ( { model | isCroppingSuccessful = True, crop = newCrop }, Navigation.newUrl "#processing" )

        StoneDetected detection ->
            let
                newDetections =
                    Array.push detection model.detections
            in
                ( { model | detections = newDetections }, Cmd.none )

        ProcessingSuccessful isSuccessful ->
            let
                newBoard =
                    detectionsToStones model.detections
            in
                ( { model | isProcessingSuccessful = isSuccessful, board = newBoard }, Cmd.none )

        DetectionsToStones ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PT.cameraStarted CameraStarted
        , PT.cameraStopped CameraStopped
        , PT.inputSuccessful InputSuccessful
        , PT.croppingSuccessful CroppingSuccessful
        , PT.stoneDetected StoneDetected
        , PT.processingSuccessful ProcessingSuccessful
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ classList [ ( "container-app", True ) ] ]
        [ page model
        , canvas
            [ id "canvas-input"
            , classList [ ( "canvas", True ), ( "canvas--visible", model.route == RouteCrop ) ]
            ]
            []
        , canvas
            [ id "canvas-output"
            , classList [ ( "canvas", True ), ( "canvas--visible", model.route == RouteProcessing ) ]
            ]
            []
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

        RouteProcessing ->
            viewProcessing model

        RouteScore ->
            viewScore model

        RouteNotFound ->
            div [] []



-- VIEW GENERAL


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


viewProcessing : Model -> Html Msg
viewProcessing model =
    let
        overlay =
            case model.isCroppingSuccessful of
                True ->
                    [ buttonClose True
                    , div
                        [ classList [ ( "detections", True ) ]
                        , style
                            [ ( "top", intToPixels model.crop.y )
                            , ( "left", intToPixels model.crop.x )
                            , ( "width", intToPixels model.crop.width )
                            , ( "height", intToPixels model.crop.height )
                            ]
                        ]
                        (List.map
                            (viewDetection model)
                            (Array.toList model.detections)
                        )
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
                        DetectionsToStones
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


viewDetection : Model -> PT.Detection -> Html Msg
viewDetection model detection =
    div
        [ classList
            [ ( "detection", True )
            , ( "detection--black", detection.color == "black" )
            , ( "detection--white", detection.color == "white" )
            ]
        , style
            [ ( "top", intToPixels detection.y )
            , ( "left", intToPixels detection.x )
            , ( "width", intToPixels detection.width )
            , ( "height", intToPixels detection.height )
            ]
        ]
        []


viewScore : Model -> Html Msg
viewScore model =
    div
        [ classList [ ( "container-score", True ) ] ]
        [ viewBoard model
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
    div
        [ classList
            [ ( "board-column", True )
            , ( "board-column--19x19", model.boardSize == Nineteen )
            , ( "board-column--13x13", model.boardSize == Thirteen )
            , ( "board-column--9x9", model.boardSize == Nine )
            ]
        ]
        [ span [ classList [ ( "board-point", True ), ( "board-point--star", isStar model x y ) ] ] []
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
