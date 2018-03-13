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
import Dict exposing (Dict)


main : Program Flags Model Msg
main =
    Navigation.programWithFlags ChangeLocation { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { config : Config
    , route : Route
    , location : Location
    , rem : Float
    , imageId : String
    , imageSource : Maybe Image
    , amountStones : Int
    , boardSizes : List BoardSize
    , boardSize : BoardSize
    , isResizing : Bool
    , isDragging : Bool
    , cropId : String
    , cropPosition : Maybe ScreenPosition
    , cropResizeStart : Maybe ScreenPosition
    , cropSize : Maybe ElementSize
    , pixelsSource : Maybe Pixels
    , pixelsProcessed : Maybe (Dict Int Pixels)
    , stars19x19 : List Star
    , stars13x13 : List Star
    , stars9x9 : List Star
    , board : Board
    }


type alias Flags =
    { version : String, baseURL : String }


type alias Config =
    { version : String, baseURL : String }


type alias WindowSize =
    { width : Float, height : Float }


type alias Image =
    { dataArray : Array Int, dataBase64 : String, width : Int, height : Int }


type TextDirection
    = ToLeft
    | ToRight


type alias ScreenPosition =
    { x : Int, y : Int }


type alias TouchScreenPositions =
    { touches : Array ScreenPosition }


type alias ElementSize =
    { width : Float, height : Float, maxWidth : Float, maxHeight : Float }


type alias Pixels =
    Array Int


type alias PixelMap =
    Dict Int Pixels


type alias CanvasPixels =
    { width : Float, height : Float, pixels : Pixels }


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
            , rem = 0
            , imageId = "image-source"
            , imageSource = Nothing
            , amountStones = 284
            , boardSizes = [ Nineteen, Thirteen, Nine ]
            , boardSize = Nineteen
            , isResizing = False
            , isDragging = False
            , cropId = "crop-selection"
            , cropPosition = Nothing
            , cropResizeStart = Nothing
            , cropSize = Nothing
            , pixelsSource = Nothing
            , pixelsProcessed = Nothing
            , stars19x19 = stars19x19
            , stars13x13 = stars13x13
            , stars9x9 = stars9x9
            , board = Array.empty
            }
    in
        ( model, PT.takeWindowSizeSnapshot True )



-- MODEL HELPERS


groupPixels : PixelMap -> Pixels -> Int -> PixelMap
groupPixels dict pixels index =
    let
        value =
            Maybe.withDefault 0 (Array.get index pixels)
    in
        case index % 4 of
            0 ->
                let
                    newDict =
                        Dict.insert index (Array.fromList [ value ]) dict
                in
                    groupPixels newDict pixels (index + 1)

            _ ->
                let
                    group =
                        Maybe.withDefault Array.empty (Dict.get (index - (index % 4)) dict)

                    newDict =
                        Dict.insert index (Array.push value group) dict
                in
                    if (index + 1) < (Array.length pixels) then
                        groupPixels newDict pixels (index + 1)
                    else
                        let
                            log =
                                Debug.log "grouped" index
                        in
                            newDict


pixelsToPixelMap : Pixels -> PixelMap
pixelsToPixelMap pixels =
    groupPixels Dict.empty pixels 0


ungroupPixels : Pixels -> PixelMap -> Int -> Pixels
ungroupPixels list pixels index =
    let
        group =
            Maybe.withDefault Array.empty (Dict.get index pixels)

        newList =
            Array.append list group
    in
        if (index + 1) < (Dict.size pixels) then
            ungroupPixels newList pixels (index + 1)
        else
            let
                log =
                    Debug.log "ungrouped" index
            in
                newList


pixelMapToPixels : PixelMap -> Pixels
pixelMapToPixels pixels =
    ungroupPixels Array.empty pixels 0


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



-- DECODERS


decodeScreenPosition : JD.Decoder ScreenPosition
decodeScreenPosition =
    JD.map2 ScreenPosition
        (JD.field "pageX" JD.int)
        (JD.field "pageY" JD.int)


decodeTouchScreenPosition : JD.Decoder ScreenPosition
decodeTouchScreenPosition =
    JD.at [ "touches", "0" ] decodeScreenPosition



-- PIXEL PROCESSING


averageRGB : Int -> Pixels -> Pixels
averageRGB index pixels =
    let
        red =
            Maybe.withDefault 0 (Array.get 0 pixels)

        green =
            Maybe.withDefault 0 (Array.get 1 pixels)

        blue =
            Maybe.withDefault 0 (Array.get 2 pixels)

        alpha =
            Maybe.withDefault 0 (Array.get 3 pixels)

        average =
            round (toFloat (red + green + blue) / 3)
    in
        Array.fromList [ average, average, average, alpha ]


greyscale : PixelMap -> Pixels
greyscale pixels =
    Dict.map averageRGB pixels
        |> pixelMapToPixels



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
    | WindowSizeSnapshotted PT.WindowSizeData
    | StartCamera
    | TakePhoto
    | ImageSelected
    | ImageProcessed PT.ImageData
    | NewAmountStones String
    | NewBoardSize String
    | StartResizing ScreenPosition
    | Resizing ScreenPosition
    | StartDragging ScreenPosition
    | Dragging ScreenPosition
    | StopInput
    | CropPhoto
    | PixelsProcessed PT.PixelsData


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        ChangePath path ->
            ( model, Navigation.newUrl path )

        ChangeLocation location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model
                    | route = newRoute
                    , location = location
                  }
                , Cmd.none
                )

        WindowSizeSnapshotted size ->
            let
                newRem =
                    (Basics.min size.width size.height) / 100

                width =
                    --size.width / 2
                    900.0

                height =
                    --size.height / 2
                    903.0

                x =
                    ((size.width / 2) - (width / 2)) - newRem

                y =
                    ((size.height / 2) - (height / 2)) - newRem

                maxWidth =
                    size.width - x

                maxHeight =
                    size.height - y

                newCropPosition =
                    ScreenPosition (round x) (round y)

                newCropSize =
                    ElementSize width height maxWidth maxHeight
            in
                ( { model | rem = newRem, cropPosition = Just newCropPosition, cropSize = Just newCropSize }, Cmd.none )

        StartCamera ->
            ( model, PT.startCamera True )

        TakePhoto ->
            ( model, PT.takePhoto True )

        ImageSelected ->
            ( model, PT.fileSelected model.imageId )

        ImageProcessed image ->
            ( { model | imageSource = Just image }, Navigation.newUrl "#crop" )

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

        StartResizing position ->
            ( { model | isResizing = True, cropResizeStart = Just position }, Cmd.none )

        Resizing position ->
            case model.isResizing of
                True ->
                    case model.cropSize of
                        Just size ->
                            let
                                startPosition =
                                    Maybe.withDefault (ScreenPosition 0 0) model.cropResizeStart

                                newWidth =
                                    size.width + (toFloat (position.x - startPosition.x) / 10)

                                newHeight =
                                    size.height + (toFloat (position.y - startPosition.y) / 10)

                                newSize =
                                    { size | width = newWidth, height = newHeight }
                            in
                                ( { model | cropSize = Just newSize }, Cmd.none )

                        Nothing ->
                            ( model, Cmd.none )

                False ->
                    ( model, Cmd.none )

        StartDragging position ->
            ( { model | isDragging = True, cropPosition = Just position }, Cmd.none )

        Dragging position ->
            case model.isDragging of
                True ->
                    ( { model | cropPosition = Just position }, Cmd.none )

                False ->
                    ( model, Cmd.none )

        StopInput ->
            ( { model | isResizing = False, isDragging = False }, Cmd.none )

        CropPhoto ->
            let
                cropPosition =
                    case model.cropPosition of
                        Just position ->
                            position

                        Nothing ->
                            ScreenPosition 0 0

                cropSize =
                    case model.cropSize of
                        Just size ->
                            size

                        Nothing ->
                            ElementSize 0 0 0 0
            in
                ( model
                , PT.cropPhoto
                    (PT.CropData
                        model.cropId
                        cropPosition.x
                        cropPosition.y
                        cropSize.width
                        cropSize.height
                    )
                )

        PixelsProcessed pixels ->
            let
                processedPixels =
                    pixelsToPixelMap pixels

                canvasPixels =
                    CanvasPixels 0 0 (greyscale processedPixels)
            in
                ( { model | pixelsSource = Just pixels, pixelsProcessed = Just processedPixels }, PT.drawPixels canvasPixels )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ PT.snapshotWindowSize WindowSizeSnapshotted
        , PT.processImage ImageProcessed
        , PT.processPixels PixelsProcessed
        ]



-- VIEW


view : Model -> Html Msg
view model =
    div [ classList [ ( "container-app", True ) ] ]
        [ page model
        , canvas
            [ id "canvas"
            , classList [ ( "canvas", True ), ( "canvas--visible", model.route == RouteCrop ) ]
            ]
            []
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


viewImage : Image -> Html Msg
viewImage image =
    img [ src image.dataBase64 ] []



-- VIEW PARTS


attributesDragMiddle : List (Html.Attribute Msg)
attributesDragMiddle =
    [ classList [ ( "drag-area", True ), ( "drag-area--middle", True ) ]
    , on "mousedown" (JD.map StartDragging decodeScreenPosition)
    , on "mousemove" (JD.map Dragging decodeScreenPosition)
    , onWithOptions "touchstart" (Options False True) (JD.map StartDragging decodeTouchScreenPosition)
    , onWithOptions "touchmove" (Options False True) (JD.map Dragging decodeTouchScreenPosition)
    ]


attributesDragCorner : List (Html.Attribute Msg)
attributesDragCorner =
    [ classList [ ( "drag-area", True ), ( "drag-area--corner", True ) ]
    , on "mousedown" (JD.map StartResizing decodeScreenPosition)
    , on "mousemove" (JD.map Resizing decodeScreenPosition)
    , onWithOptions "touchstart" (Options False True) (JD.map StartResizing decodeTouchScreenPosition)
    , onWithOptions "touchmove" (Options False True) (JD.map Resizing decodeTouchScreenPosition)
    ]


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
                [ label [ for model.imageId ]
                    [ viewIconText "file" [] "File" ToRight True
                    , input [ id model.imageId, type_ "file", on "change" (JD.succeed ImageSelected) ] []
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


viewPhoto : Model -> Html Msg
viewPhoto model =
    div [ classList [ ( "container-photo", True ) ] ]
        [ video [ id "video", classList [ ( "video", True ) ], onClick TakePhoto ] []
        , buttonClose False
        , viewIconTextLink
            "camera"
            [ ( "camera", True ), ( "camera--start", True ) ]
            "Start camera"
            ToRight
            StartCamera
        , viewIconText
            "info"
            [ ( "camera", True ), ( "camera--info", True ) ]
            "Click/tap anywhere to take a photo"
            ToRight
            False
        ]


viewCrop : Model -> Html Msg
viewCrop model =
    let
        newPosition =
            case model.cropPosition of
                Just position ->
                    [ ( "top", (toString position.y) ++ "px" ), ( "left", (toString position.x) ++ "px" ) ]

                Nothing ->
                    []

        newSize =
            case model.cropSize of
                Just size ->
                    [ ( "width", (toString size.width) ++ "px" )
                    , ( "height", (toString size.height) ++ "px" )
                    , ( "max-width", (toString size.maxWidth) ++ "px" )
                    , ( "max-height", (toString size.maxHeight) ++ "px" )
                    ]

                Nothing ->
                    []

        styles =
            newPosition ++ newSize

        content =
            case model.imageSource of
                Just image ->
                    [ buttonClose True
                    , viewIconText
                        "info"
                        [ ( "crop", True ), ( "crop--overlay", True ) ]
                        "Crop the image so only the board remains"
                        ToRight
                        False
                    , div
                        [ id "crop-selection"
                        , classList [ ( "crop-selection", True ) ]
                        , style styles
                        ]
                        [ div
                            attributesDragMiddle
                            []
                        , div
                            attributesDragCorner
                            []
                        ]
                    , viewIconTextLink
                        "crop"
                        [ ( "crop", True ), ( "crop--overlay", True ), ( "crop--start", True ) ]
                        "Crop"
                        ToRight
                        CropPhoto
                    ]

                Nothing ->
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
            [ classList [ ( "container-crop", True ) ]
            , on "mouseup" (JD.succeed StopInput)
            , onWithOptions "touchend" (Options False True) (JD.succeed StopInput)
            ]
            content


viewProcessing : Model -> Html Msg
viewProcessing model =
    let
        content =
            case model.pixelsSource of
                Just pixels ->
                    [ buttonClose True
                    ]

                Nothing ->
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
            content


viewScore : Model -> Html Msg
viewScore model =
    div [ classList [ ( "container-score", True ) ] ]
        [ viewBoard model
        ]


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
