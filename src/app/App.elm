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
    , isColorPickingSuccessful : Bool
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


type alias DetectionMeta =
    { averageWidth : Int
    , averageHeight : Int
    , minimumWidth : Int
    , minimumHeight : Int
    , minimumX : Int
    , minimumY : Int
    , maximumX : Int
    , maximumY : Int
    }


type alias Star =
    { x : Int, y : Int }


type alias StoneMatrix =
    { startX : Int
    , startY : Int
    , currentX : Int
    , currentY : Int
    , endX : Int
    , endY : Int
    }


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
            , isColorPickingSuccessful = False
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


minimumBoardX : List PT.Detection -> Int -> Int
minimumBoardX detections averageWidth =
    List.map .x detections
        |> List.sort
        |> List.filter (\x -> x <= averageWidth)
        |> List.maximum
        |> Maybe.withDefault 0


maximumBoardX : List PT.Detection -> Int -> Int -> Int -> Int
maximumBoardX detections averageWidth minimumX boardWidth =
    List.map .x detections
        |> List.sort
        |> List.reverse
        |> List.filter (\x -> (boardWidth - (x + minimumX)) <= averageWidth)
        |> List.minimum
        |> Maybe.withDefault -1


minimumBoardY : List PT.Detection -> Int -> Int
minimumBoardY detections averageHeight =
    List.map .y detections
        |> List.sort
        |> List.filter (\y -> y <= averageHeight)
        |> List.maximum
        |> Maybe.withDefault 0


maximumBoardY : List PT.Detection -> Int -> Int -> Int -> Int
maximumBoardY detections averageHeight minimumY boardHeight =
    List.map .y detections
        |> List.sort
        |> List.reverse
        |> List.filter (\y -> (boardHeight - (y + minimumY)) <= averageHeight)
        |> List.minimum
        |> Maybe.withDefault -1


detectionColorToStoneColor : String -> StoneColor
detectionColorToStoneColor color =
    case color of
        "black" ->
            Black

        "white" ->
            White

        _ ->
            Black


stoneMatrixToStones : StoneMatrix -> StoneColor -> Array Stone -> Array Stone
stoneMatrixToStones matrix color stones =
    let
        newStones =
            Array.push (Stone matrix.currentX matrix.currentY 4 color) stones

        newX =
            case matrix.currentX + 1 > matrix.endX of
                True ->
                    matrix.startX

                False ->
                    matrix.currentX + 1

        newY =
            case matrix.currentX + 1 > matrix.endX of
                True ->
                    matrix.currentY + 1

                False ->
                    matrix.currentY

        newMatrix =
            { matrix | currentX = newX, currentY = newY }
    in
        case newY > matrix.endY of
            True ->
                newStones

            False ->
                stoneMatrixToStones newMatrix color newStones


detectionToStones : DetectionMeta -> PT.Detection -> List Stone
detectionToStones meta detection =
    let
        startX =
            toFloat (detection.x - meta.minimumX - (meta.averageWidth - detection.width))
                / toFloat meta.averageWidth
                |> floor
                |> Basics.max 1
                |> Basics.min 19

        startY =
            toFloat (detection.y - meta.minimumY - (meta.averageHeight - detection.height))
                / toFloat meta.averageHeight
                |> floor
                |> Basics.max 1
                |> Basics.min 19

        endX =
            if detection.width < meta.averageWidth && detection.width >= meta.minimumWidth then
                startX
            else
                toFloat detection.width
                    / toFloat meta.averageWidth
                    |> floor
                    |> Basics.min 19

        endY =
            if detection.height < meta.averageHeight && detection.height >= meta.minimumHeight then
                startY
            else
                toFloat detection.height
                    / toFloat meta.averageHeight
                    |> floor
                    |> Basics.min 19

        matrix =
            StoneMatrix startX startY startX startY endX endY

        color =
            detectionColorToStoneColor detection.color
    in
        stoneMatrixToStones matrix color Array.empty
            |> Array.toList


detectionsToStones : List PT.Detection -> PT.Crop -> Board
detectionsToStones detections crop =
    let
        averageWidth =
            ceiling <| averageStoneWidth detections

        averageHeight =
            ceiling <| averageStoneHeight detections

        minimumWidth =
            List.map .width detections
                |> List.minimum
                |> Maybe.withDefault (ceiling (toFloat averageWidth * 0.6))

        minimumHeight =
            List.map .width detections
                |> List.minimum
                |> Maybe.withDefault (ceiling (toFloat averageWidth * 0.6))

        minimumX =
            minimumBoardX detections averageWidth

        minimumY =
            minimumBoardY detections averageHeight

        maximumX =
            maximumBoardX detections averageWidth minimumX crop.width

        maximumY =
            maximumBoardY detections averageHeight minimumY crop.height

        meta =
            DetectionMeta
                averageWidth
                averageHeight
                minimumWidth
                minimumHeight
                minimumX
                minimumY
                maximumX
                maximumY

        log =
            Debug.log "meta" meta
    in
        List.map (detectionToStones meta) detections
            |> List.concat
            |> Array.fromList


stonesForPosition : Model -> Int -> Int -> List Stone
stonesForPosition model x y =
    model.board
        |> Array.toList
        |> List.filter (\z -> z.x == x && z.y == y)


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
    | RouteColor
    | RouteProcessing
    | RouteScore
    | RouteNotFound


matchers : UP.Parser (Route -> a) a
matchers =
    UP.oneOf
        [ UP.map RouteLanding UP.top
        , UP.map RoutePhoto (UP.s "photo")
        , UP.map RouteCrop (UP.s "crop")
        , UP.map RouteColor (UP.s "color")
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
    | PickColors
    | ColorPickingSuccessful Bool
    | StoneDetected PT.Detection
    | ProcessingSuccessful Bool


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

                        RouteColor ->
                            PT.startPickingColors True

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
            ( { model | isCroppingSuccessful = True, crop = newCrop }, Navigation.newUrl "#color" )

        PickColors ->
            ( model, PT.pickColors True )

        ColorPickingSuccessful isSuccessful ->
            ( { model | isColorPickingSuccessful = isSuccessful }, Navigation.newUrl "#processing" )

        StoneDetected detection ->
            let
                newDetections =
                    Array.push detection model.detections
            in
                ( { model | detections = newDetections }, Cmd.none )

        ProcessingSuccessful isSuccessful ->
            let
                newBoard =
                    detectionsToStones (Array.toList model.detections) model.crop
            in
                ( { model | isProcessingSuccessful = isSuccessful, board = newBoard }, Cmd.none )



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
        , canvas
            [ id "canvas-color-black"
            , classList [ ( "canvas", True ), ( "canvas--visible", model.route == RouteColor ) ]
            ]
            []
        , canvas
            [ id "canvas-color-white"
            , classList [ ( "canvas", True ), ( "canvas--visible", model.route == RouteColor ) ]
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

        RouteColor ->
            div [] []

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


viewColor : Model -> Html Msg
viewColor model =
    let
        overlay =
            case model.isCroppingSuccessful of
                True ->
                    [ buttonClose True
                    , viewIconText
                        "info"
                        [ ( "crop", True ), ( "crop--overlay", True ), ( "crop--info", True ) ]
                        "Select the darkest white and lightest black stones"
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
        stones =
            stonesForPosition model x y

        isStone =
            List.length stones > 0

        isBlack =
            List.length stones == 1 && List.any (\x -> x.color == Black) stones

        isWhite =
            List.length stones == 1 && List.any (\x -> x.color == White) stones

        isConflict =
            List.length stones > 1
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
                    [ ( "board-stone", isStone )
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
