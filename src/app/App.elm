module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Navigation exposing (Location)
import UrlParser as UP
import Json.Decode as JD
import Ports as PT
import Svg exposing (svg, use)
import Svg.Attributes as SA exposing (class, xlinkHref)
import Dict exposing (Dict)
import Dict.Extra
import Array exposing (Array)


main : Program Never Model Msg
main =
    Navigation.program ChangeLocation { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { route : Route
    , location : Location
    , isLoading : Bool
    , loadingMessage : Maybe String
    , messages : Dict String Message
    , boardSizes : List BoardSize
    , boardSize : BoardSize
    , komi : Float
    , prisonersBlack : Int
    , prisonersWhite : Int
    , areDeadRemoved : Bool
    , showHelp : Bool
    , isVideoPlaying : Bool
    , isInputSuccessful : Bool
    , isCroppingSuccessful : Bool
    , isPickingBlackSuccessful : Bool
    , isPickingWhiteSuccessful : Bool
    , isProcessingSuccessful : Bool
    , stars19x19 : List Star
    , stars13x13 : List Star
    , stars9x9 : List Star
    , board : Board
    , dead : Dict BoardPosition Dead
    , territory : Dict BoardPosition Territory
    , state : State
    , tool : Tool
    , showingTools : Bool
    , showingScore : Bool
    , scoreBlack : Float
    , scoreWhite : Float
    }


type TextDirection
    = ToLeft
    | ToRight


type ViewSize
    = Medium
    | Big


type CropShape
    = Rectangle
    | Circle


type alias Message =
    { isVisible : Bool, content : List String }


type alias Probability =
    { stone : Int, black : Int, white : Int }


type alias Star =
    { x : Int, y : Int }


type StoneColor
    = White
    | Black
    | Conflict
    | Empty


type alias Stone =
    { isStone : Bool, color : StoneColor }


type alias Prisoner =
    StoneColor


type Edge
    = BoardEdge
    | ColorEdge StoneColor
    | NotEdge


type Direction
    = Up
    | Right
    | Down
    | Left


type BoardSize
    = Nineteen
    | Thirteen
    | Nine


type alias BoardPosition =
    ( Int, Int )


type alias Board =
    Dict BoardPosition Stone


type alias Territory =
    { isTerritory : Bool, color : StoneColor }


type alias Dead =
    { isDead : Bool, color : StoneColor }


type State
    = Editing
    | Scoring


type Tool
    = DeadBlack
    | DeadWhite
    | Alive
    | AddBlack
    | AddWhite
    | Remove
    | NoTool


init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            parseLocation location

        model =
            { route = currentRoute
            , location = location
            , isLoading = False
            , loadingMessage = Nothing
            , messages = messages
            , boardSizes = [ Nineteen, Thirteen, Nine ]
            , boardSize = Nineteen
            , komi = 6.5
            , prisonersBlack = 0
            , prisonersWhite = 0
            , areDeadRemoved = False
            , showHelp = True
            , isVideoPlaying = False
            , isInputSuccessful = False
            , isCroppingSuccessful = False
            , isPickingBlackSuccessful = False
            , isPickingWhiteSuccessful = False
            , isProcessingSuccessful = False
            , stars19x19 = stars19x19
            , stars13x13 = stars13x13
            , stars9x9 = stars9x9
            , board = Dict.empty
            , dead = Dict.empty
            , territory = Dict.empty
            , state = Editing
            , tool = NoTool
            , showingTools = False
            , showingScore = False
            , scoreBlack = 0
            , scoreWhite = 0
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


probabilityToStone : Int -> String -> Probability -> Stone
probabilityToStone maximumProbability key probability =
    let
        probabilityPercentage =
            toFloat probability.stone / toFloat maximumProbability

        isStone =
            probabilityPercentage > 0.25

        color =
            if
                isStone
                    && probability.black
                    > 0
                    && probability.black
                    > probability.white
            then
                Black
            else if
                isStone
                    && probability.white
                    > 0
                    && probability.white
                    > probability.black
            then
                White
            else if
                isStone
                    && probability.black
                    == probability.white
            then
                Conflict
            else
                Empty
    in
        Stone isStone color


boardKeyToBoardPosition : String -> BoardPosition
boardKeyToBoardPosition key =
    let
        values =
            String.split "-" key
                |> Array.fromList

        y =
            Array.get 0 values
                |> Maybe.withDefault "-1"
                |> String.toInt
                |> Result.withDefault -1

        x =
            Array.get 1 values
                |> Maybe.withDefault "-1"
                |> String.toInt
                |> Result.withDefault -1
    in
        ( y, x )


isPositionStar : List Star -> Int -> Int -> Bool
isPositionStar stars x y =
    List.any (\z -> z == (Star x y)) stars



-- DATA


messages : Dict String Message
messages =
    [ ( "photo"
      , Message True
            [ "Press anywhere to take a photo"
            , "Put the camera directly above the board, not at an angle"
            ]
      )
    , ( "crop"
      , Message True
            [ "Crop the image so only the board remains"
            , "Try to be precise, it's important for getting accurate results"
            ]
      )
    , ( "black"
      , Message True
            [ "Select the LIGHTEST BLACK stone"
            , "Make sure the preview shows exactly one stone and nothing else, not even the board"
            , "The lightest one is usually the one with the most reflection on it"
            ]
      )
    , ( "white"
      , Message True
            [ "Select the DARKEST WHITE stone"
            , "Make sure the preview shows exactly one stone and nothing else, not even the board"
            , "The darkest one is usually the one with the most shadow on it"
            ]
      )
    , ( "processing"
      , Message True
            [ "This is how I detected the stones on the board"
            , "If you see large errors, you should repeat the previous steps"
            , "You can make corrections in the next step"
            ]
      )
    , ( "score"
      , Message True
            [ "Please mark all dead stones by using the approriate tools"
            , "To use a tool, select it and then press anywhere on the board"
            ]
      )
    ]
        |> Dict.fromList


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



-- CALCULATION


findEdgeFromPosition : Board -> BoardPosition -> Direction -> Edge
findEdgeFromPosition board position direction =
    let
        y =
            Tuple.first position

        x =
            Tuple.second position

        nextPosition =
            case direction of
                Up ->
                    ( (Basics.max 1 (y - 1)), x )

                Right ->
                    ( y, (Basics.min 19 (x + 1)) )

                Down ->
                    ( (Basics.min 19 (y + 1)), x )

                Left ->
                    ( y, (Basics.max 1 (x - 1)) )

        nextStone =
            Dict.get nextPosition board
                |> Maybe.withDefault (Stone False Empty)

        edge =
            case direction of
                Up ->
                    if y == 1 then
                        BoardEdge
                    else if nextStone.isStone then
                        ColorEdge nextStone.color
                    else
                        NotEdge

                Right ->
                    if x == 19 then
                        BoardEdge
                    else if nextStone.isStone then
                        ColorEdge nextStone.color
                    else
                        NotEdge

                Down ->
                    if y == 19 then
                        BoardEdge
                    else if nextStone.isStone then
                        ColorEdge nextStone.color
                    else
                        NotEdge

                Left ->
                    if x == 1 then
                        BoardEdge
                    else if nextStone.isStone then
                        ColorEdge nextStone.color
                    else
                        NotEdge
    in
        case edge of
            NotEdge ->
                findEdgeFromPosition board nextPosition direction

            _ ->
                edge


isNotDead : Dict BoardPosition Dead -> BoardPosition -> Stone -> Bool
isNotDead dead position stone =
    Dict.get position dead
        |> Maybe.withDefault (Dead False Empty)
        |> .isDead
        |> not


isSpaceOrDead : Dict BoardPosition Dead -> BoardPosition -> Stone -> Bool
isSpaceOrDead dead position stone =
    let
        isDead =
            Dict.get position dead
                |> Maybe.withDefault (Dead False Empty)
                |> .isDead
    in
        not stone.isStone || isDead


isTerritoryForWho : Board -> BoardPosition -> Stone -> Territory
isTerritoryForWho board position stone =
    let
        edgeUp =
            findEdgeFromPosition board position Up

        edgeRight =
            findEdgeFromPosition board position Right

        edgeDown =
            findEdgeFromPosition board position Down

        edgeLeft =
            findEdgeFromPosition board position Left

        edges =
            [ edgeUp, edgeRight, edgeDown, edgeLeft ]

        blackEdges =
            List.filter (\x -> x == ColorEdge Black) edges
                |> List.length

        whiteEdges =
            List.filter (\x -> x == ColorEdge White) edges
                |> List.length
    in
        if blackEdges > 0 && whiteEdges == 0 then
            Territory True Black
        else if whiteEdges > 0 && blackEdges == 0 then
            Territory True White
        else if blackEdges > 0 && whiteEdges > 0 then
            Territory True Empty
        else
            Territory False Empty


findTerritories : Board -> Dict BoardPosition Dead -> Dict BoardPosition Territory
findTerritories board dead =
    let
        boardWithoutDead =
            Dict.filter (isNotDead dead) board

        spaces =
            Dict.filter (isSpaceOrDead dead) board

        territory =
            Dict.map (isTerritoryForWho boardWithoutDead) spaces
    in
        territory


calculateScoreBlack : Dict BoardPosition Territory -> Dict BoardPosition Dead -> Int -> Float
calculateScoreBlack territory dead prisoners =
    let
        territoryBlack =
            Dict.filter (\position territory -> territory.color == Black) territory
                |> Dict.size

        deadWhite =
            Dict.filter (\position dead -> dead.color == White) dead
                |> Dict.size
    in
        toFloat <| territoryBlack + deadWhite + prisoners


calculateScoreWhite : Dict BoardPosition Territory -> Dict BoardPosition Dead -> Int -> Float -> Float
calculateScoreWhite territory dead prisoners komi =
    let
        territoryWhite =
            Dict.filter (\position territory -> territory.color == White) territory
                |> Dict.size

        deadBlack =
            Dict.filter (\position dead -> dead.color == Black) dead
                |> Dict.size
    in
        (toFloat <| territoryWhite + deadBlack + prisoners) + komi



-- DECODERS


decoderProbability : JD.Decoder Probability
decoderProbability =
    JD.map3 Probability
        (JD.field "stone" JD.int)
        (JD.field "black" JD.int)
        (JD.field "white" JD.int)


decoderProbabilities : JD.Decoder (Dict String Probability)
decoderProbabilities =
    JD.dict decoderProbability


decodeProbabilities : JD.Value -> Result String (Dict String Probability)
decodeProbabilities =
    JD.decodeValue decoderProbabilities



-- ROUTING


type Route
    = RouteLanding
    | RouteSettings
    | RouteSource
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
        , UP.map RouteSettings (UP.s "settings")
        , UP.map RouteSource (UP.s "source")
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
    | CloseMessage String (List String)
    | StartCamera
    | NewFile
    | NewBoardSize String
    | NewKomi String
    | NewPrisonersBlack String
    | NewPrisonersWhite String
    | NewAreDeadRemoved Bool
    | NewShowHelp Bool
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
    | ProcessingSuccessful (Result String (Dict String Probability))
    | NewState State
    | NewTool Tool
    | UseToolAt BoardPosition


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

        CloseMessage name content ->
            let
                newMessage =
                    Message False content

                newMessages =
                    Dict.insert name newMessage model.messages
            in
                ( { model | messages = newMessages }, Cmd.none )

        StartCamera ->
            ( { model | isLoading = True, loadingMessage = Just "Starting camera" }, PT.startCamera True )

        NewFile ->
            ( model, PT.useFile "file-input" )

        NewBoardSize boardSize ->
            let
                newBoardSize =
                    stringToBoardSize boardSize
            in
                ( { model | boardSize = newBoardSize }, Cmd.none )

        NewKomi komi ->
            let
                newKomi =
                    Result.withDefault 0 (String.toFloat komi)
            in
                ( { model | komi = newKomi }, Cmd.none )

        NewPrisonersBlack prisoners ->
            let
                newPrisoners =
                    Result.withDefault 0 (String.toInt prisoners)
            in
                ( { model | prisonersBlack = newPrisoners }, Cmd.none )

        NewPrisonersWhite prisoners ->
            let
                newPrisoners =
                    Result.withDefault 0 (String.toInt prisoners)
            in
                ( { model | prisonersWhite = newPrisoners }, Cmd.none )

        NewAreDeadRemoved removed ->
            ( { model | areDeadRemoved = removed }, Cmd.none )

        NewShowHelp show ->
            ( { model | showHelp = show }, Cmd.none )

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

        ProcessingSuccessful (Ok probabilities) ->
            let
                maximumProbability =
                    Dict.values probabilities
                        |> List.map .stone
                        |> List.maximum
                        |> Maybe.withDefault 0

                stones =
                    Dict.map (probabilityToStone maximumProbability) probabilities

                board =
                    Dict.Extra.mapKeys boardKeyToBoardPosition stones
            in
                ( { model | isProcessingSuccessful = True, board = board }, Cmd.none )

        ProcessingSuccessful (Err _) ->
            ( model, Cmd.none )

        NewState state ->
            let
                showingTools =
                    case state of
                        Editing ->
                            not model.showingTools

                        Scoring ->
                            False

                showingScore =
                    case state of
                        Scoring ->
                            not model.showingScore

                        Editing ->
                            False

                territory =
                    case state of
                        Scoring ->
                            findTerritories model.board model.dead

                        Editing ->
                            model.territory

                scoreBlack =
                    calculateScoreBlack territory model.dead model.prisonersBlack

                scoreWhite =
                    calculateScoreWhite territory model.dead model.prisonersWhite model.komi
            in
                ( { model
                    | state = state
                    , showingTools = showingTools
                    , showingScore = showingScore
                    , territory = territory
                    , scoreBlack = scoreBlack
                    , scoreWhite = scoreWhite
                  }
                , Cmd.none
                )

        NewTool tool ->
            ( { model | tool = tool, showingTools = False }, Cmd.none )

        UseToolAt position ->
            let
                board =
                    case model.tool of
                        AddBlack ->
                            Dict.insert position (Stone True Black) model.board

                        AddWhite ->
                            Dict.insert position (Stone True White) model.board

                        Remove ->
                            Dict.insert position (Stone False Empty) model.board

                        _ ->
                            model.board

                dead =
                    case model.tool of
                        DeadBlack ->
                            Dict.insert position (Dead True Black) model.dead

                        DeadWhite ->
                            Dict.insert position (Dead True White) model.dead

                        Alive ->
                            Dict.remove position model.dead

                        _ ->
                            model.dead
            in
                ( { model | board = board, dead = dead }, Cmd.none )



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
        , PT.processingSuccessful (decodeProbabilities >> ProcessingSuccessful)
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

        RouteSettings ->
            viewSettings model

        RouteSource ->
            viewSource model

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


viewIcon : String -> List ( String, Bool ) -> Html Msg
viewIcon name classesExtra =
    let
        classes =
            [ ( "icon", True ) ] ++ classesExtra
    in
        svg [ SA.class (classString classes) ]
            [ use [ xlinkHref ("#" ++ name) ] [] ]


viewIconText : String -> List ( String, Bool ) -> String -> TextDirection -> Bool -> ViewSize -> Html Msg
viewIconText name classesExtra value direction isClickable size =
    let
        classes =
            [ ( "container-icon-text", True ), ( "link-fake", isClickable ) ] ++ classesExtra

        textClass =
            case size of
                Medium ->
                    "text-icon--medium"

                Big ->
                    "text-icon--big"

        iconClass =
            case size of
                Medium ->
                    "icon--medium"

                Big ->
                    "icon--big"

        content =
            case direction of
                ToLeft ->
                    [ span [ classList [ ( "text-icon", True ), ( textClass, True ) ] ] [ text value ]
                    , viewIcon name [ ( iconClass, True ) ]
                    ]

                ToRight ->
                    [ viewIcon name [ ( iconClass, True ) ]
                    , span [ classList [ ( "text-icon", True ), ( textClass, True ) ] ] [ text value ]
                    ]
    in
        span [ classList classes ]
            content


viewIconTextAction : String -> List ( String, Bool ) -> String -> TextDirection -> Msg -> ViewSize -> Html Msg
viewIconTextAction name classesExtra value direction msg size =
    let
        classes =
            [ ( "container-icon-text-action", True ) ] ++ classesExtra

        textClass =
            case size of
                Medium ->
                    "text-icon--medium"

                Big ->
                    "text-icon--big"

        iconClass =
            case size of
                Medium ->
                    "icon--medium"

                Big ->
                    "icon--big"

        content =
            case direction of
                ToLeft ->
                    [ span [ classList [ ( "text-icon", True ), ( textClass, True ) ] ] [ text value ]
                    , viewIcon name [ ( iconClass, True ) ]
                    ]

                ToRight ->
                    [ viewIcon name [ ( iconClass, True ) ]
                    , span [ classList [ ( "text-icon", True ), ( textClass, True ) ] ] [ text value ]
                    ]
    in
        span [ classList classes, onClick msg ]
            content


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
                    [ ( "crop-frame-fader", True ), ( "crop-frame-fader--visible", shape == Rectangle ) ]
                ]
                []
            , div
                [ id (identifier ++ "-boundary")
                , classList
                    [ ( "crop-frame-boundary", True ) ]
                ]
                content
            ]


viewMessagePart : String -> Html Msg
viewMessagePart message =
    li [ classList [ ( "list-item", True ), ( "message-item", True ) ] ]
        [ viewIconText
            "info"
            [ ( "message-part", True ) ]
            message
            ToRight
            False
            Medium
        ]


viewMessage : String -> Bool -> Dict String Message -> Bool -> Html Msg
viewMessage name alwaysShow messages showHelp =
    let
        message =
            Dict.get name messages
                |> Maybe.withDefault (Message False [])

        isVisible =
            showHelp && message.isVisible && alwaysShow
    in
        ul [ classList [ ( "message", True ), ( "message--visible", isVisible ) ] ]
            ([ viewIconTextAction
                "close"
                [ ( "message-close", True ) ]
                "Close"
                ToLeft
                (CloseMessage name message.content)
                Medium
             ]
                ++ (List.map viewMessagePart message.content)
            )


viewClose : Bool -> Html Msg
viewClose isOverlay =
    viewIconTextAction "close" [ ( "close", True ), ( "close--overlay", isOverlay ) ] "Close" ToLeft (ChangePath "#") Big



-- VIEW CONTENT


viewLanding : Model -> Html Msg
viewLanding model =
    div [ classList [ ( "container-landing", True ) ] ]
        [ viewIconTextAction
            "start"
            [ ( "landing", True ), ( "landing--overlay", True ), ( "landing--action", True ) ]
            "Start"
            ToRight
            (ChangePath "#settings")
            Big
        , p [ classList [ ( "landing-pitch", True ) ] ]
            [ text "Automatically score your finished Go games in just a few simple steps" ]
        ]


viewSettings : Model -> Html Msg
viewSettings model =
    let
        nameAreDeadRemoved =
            if model.areDeadRemoved then
                "checked"
            else
                "box"

        nameShowHelp =
            if model.showHelp then
                "checked"
            else
                "box"
    in
        div [ classList [ ( "container-settings", True ) ] ]
            [ viewClose False
            , viewIconTextAction
                "continue"
                [ ( "settings", True ), ( "settings--overlay", True ), ( "settings--action", True ) ]
                "Continue"
                ToRight
                (ChangePath "#source")
                Big
            , div [ classList [ ( "container-settings-list", True ) ] ]
                [ ul [ classList [ ( "settings-list", True ) ] ]
                    [ li [ classList [ ( "container-select", True ), ( "list-item", True ) ] ]
                        [ viewIconText "grid" [] "Board size" ToRight False Medium
                        , select [ classList [ ( "input", True ) ], on "change" (JD.map NewBoardSize targetValue) ]
                            (List.map (viewOptionBoardSize model) model.boardSizes)
                        ]
                    , li [ classList [ ( "list-item", True ) ] ]
                        [ label [ for "prisoners-black" ]
                            [ viewIconText "white" [] "Prisoners for black" ToRight False Medium
                            , input
                                [ id "prisoners-black"
                                , classList [ ( "input", True ) ]
                                , type_ "number"
                                , onInput NewPrisonersBlack
                                , value (toString model.prisonersBlack)
                                ]
                                []
                            ]
                        ]
                    , li [ classList [ ( "list-item", True ) ] ]
                        [ label [ for "prisoners-white" ]
                            [ viewIconText "black" [] "Prisoners for white" ToRight False Medium
                            , input
                                [ id "prisoners-white"
                                , classList [ ( "input", True ) ]
                                , type_ "number"
                                , onInput NewPrisonersWhite
                                , value (toString model.prisonersWhite)
                                ]
                                []
                            ]
                        ]
                    ]
                , ul [ classList [ ( "settings-list", True ) ] ]
                    [ li [ classList [ ( "list-item", True ) ] ]
                        [ label [ for "komi" ]
                            [ viewIconText "score" [] "Komi" ToRight False Medium
                            , input
                                [ id "komi"
                                , classList [ ( "input", True ) ]
                                , type_ "number"
                                , onInput NewKomi
                                , value (toString model.komi)
                                ]
                                []
                            ]
                        ]
                    , li [ classList [ ( "list-item", True ) ] ]
                        [ viewIconTextAction
                            nameAreDeadRemoved
                            []
                            "Dead stones have been removed"
                            ToRight
                            (NewAreDeadRemoved <| not model.areDeadRemoved)
                            Medium
                        ]
                    , li [ classList [ ( "list-item", True ) ] ]
                        [ viewIconTextAction
                            nameShowHelp
                            []
                            "Show help messages"
                            ToRight
                            (NewShowHelp <| not model.showHelp)
                            Medium
                        ]
                    ]
                ]
            ]


viewOptionBoardSize : Model -> BoardSize -> Html Msg
viewOptionBoardSize model boardSize =
    option [ value (boardSizeToString boardSize), selected (model.boardSize == boardSize) ]
        [ text (boardSizeToString boardSize) ]


viewSource : Model -> Html Msg
viewSource model =
    div [ classList [ ( "container-source", True ) ] ]
        [ viewClose False
        , ul []
            [ li [ classList [ ( "container-link", True ), ( "list-item", True ) ] ]
                [ viewIconTextAction "camera" [] "Photo" ToRight (ChangePath "#photo") Big ]
            , li [ classList [ ( "container-input", True ), ( "list-item", True ) ] ]
                [ label [ for "file-input" ]
                    [ viewIconText "file" [] "File" ToRight True Big
                    , input [ id "file-input", type_ "file", on "change" (JD.succeed NewFile) ] []
                    ]
                ]
            ]
        ]


viewPhoto : Model -> Html Msg
viewPhoto model =
    let
        overlay =
            case model.isVideoPlaying of
                True ->
                    [ viewClose True
                    , viewMessage "photo" True model.messages model.showHelp
                    ]

                False ->
                    [ viewClose False
                    , viewIconTextAction
                        "camera"
                        [ ( "camera", True ), ( "camera--action", True ) ]
                        "Start camera"
                        ToRight
                        StartCamera
                        Big
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
                    [ viewClose True
                    , viewMessage "crop" True model.messages model.showHelp
                    , viewIconTextAction
                        "crop"
                        [ ( "crop", True ), ( "crop--overlay", True ), ( "crop--action", True ) ]
                        "Crop"
                        ToRight
                        CropPhoto
                        Big
                    ]

                False ->
                    [ viewClose False
                    , viewIconText
                        "info"
                        [ ( "crop", True ) ]
                        "Nothing to crop"
                        ToRight
                        False
                        Big
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
                    [ viewClose True
                    , viewMessage "black" True model.messages model.showHelp
                    , viewIconTextAction
                        "color"
                        [ ( "color", True ), ( "color--overlay", True ), ( "color--action", True ) ]
                        "Pick"
                        ToRight
                        PickBlack
                        Big
                    ]

                False ->
                    [ viewClose False
                    , viewIconText
                        "info"
                        [ ( "color", True ) ]
                        "Nothing to pick"
                        ToRight
                        False
                        Big
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
                    [ viewClose True
                    , viewMessage "white" True model.messages model.showHelp
                    , viewIconTextAction
                        "color"
                        [ ( "color", True ), ( "color--overlay", True ), ( "color--action", True ) ]
                        "Pick"
                        ToRight
                        PickWhite
                        Big
                    ]

                False ->
                    [ viewClose False
                    , viewIconText
                        "info"
                        [ ( "color", True ) ]
                        "Nothing to pick"
                        ToRight
                        False
                        Big
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
                    [ viewClose True
                    , viewMessage "processing" True model.messages model.showHelp
                    , viewIconTextAction
                        "continue"
                        [ ( "processed", True ), ( "processed--overlay", True ), ( "processed--action", True ) ]
                        "Continue"
                        ToRight
                        (ChangePath "#score")
                        Big
                    ]

                False ->
                    [ viewClose False
                    , viewIconText
                        "info"
                        [ ( "crop", True ) ]
                        "Nothing to process"
                        ToRight
                        False
                        Big
                    ]
    in
        div [ classList [ ( "container-processing", True ) ] ]
            overlay


viewScore : Model -> Html Msg
viewScore model =
    div
        [ classList [ ( "container-score", True ) ] ]
        [ viewClose True
        , viewMessage "score" (not model.areDeadRemoved) model.messages model.showHelp
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
        , ul [ classList [ ( "controls-buttons", True ) ] ]
            [ li [ classList [ ( "list-item", True ) ] ]
                [ div
                    [ classList
                        [ ( "controls-button", True )
                        , ( "controls-button--active", model.state == Scoring )
                        ]
                    , onClick (NewState Scoring)
                    ]
                    [ text "Score" ]
                ]
            , li [ classList [ ( "list-item", True ) ] ]
                [ div
                    [ classList
                        [ ( "controls-button", True )
                        , ( "controls-button--active", model.state == Editing )
                        ]
                    , onClick (NewState Editing)
                    ]
                    [ text "Tools" ]
                ]
            ]
        , ul [ classList [ ( "controls-score", True ) ] ]
            [ li [ classList [ ( "list-item", True ) ] ]
                [ viewIconText
                    "black"
                    [ ( "points", True ), ( "points--overlay", True ), ( "points--visible", model.showingScore ) ]
                    (toString model.scoreBlack)
                    ToRight
                    False
                    Big
                ]
            , li [ classList [ ( "list-item", True ) ] ]
                [ viewIconText
                    "white"
                    [ ( "points", True ), ( "points--overlay", True ), ( "points--visible", model.showingScore ) ]
                    (toString model.scoreWhite)
                    ToRight
                    False
                    Big
                ]
            ]
        , ul [ classList [ ( "controls-tools", True ) ] ]
            [ li [ classList [ ( "list-item", True ) ] ]
                [ viewIconTextAction
                    "deadblack"
                    [ ( "tool", True ), ( "tool--overlay", True ), ( "tool--visible", model.showingTools ) ]
                    "Dead black"
                    ToRight
                    (NewTool DeadBlack)
                    Big
                ]
            , li [ classList [ ( "list-item", True ) ] ]
                [ viewIconTextAction
                    "deadwhite"
                    [ ( "tool", True ), ( "tool--overlay", True ), ( "tool--visible", model.showingTools ) ]
                    "Dead white"
                    ToRight
                    (NewTool DeadWhite)
                    Big
                ]
            , li [ classList [ ( "list-item", True ) ] ]
                [ viewIconTextAction
                    "alive"
                    [ ( "tool", True ), ( "tool--overlay", True ), ( "tool--visible", model.showingTools ) ]
                    "Alive"
                    ToRight
                    (NewTool Alive)
                    Big
                ]
            , li [ classList [ ( "list-item", True ) ] ]
                [ viewIconTextAction
                    "addblack"
                    [ ( "tool", True ), ( "tool--overlay", True ), ( "tool--visible", model.showingTools ) ]
                    "Add black"
                    ToRight
                    (NewTool AddBlack)
                    Big
                ]
            , li [ classList [ ( "list-item", True ) ] ]
                [ viewIconTextAction
                    "addwhite"
                    [ ( "tool", True ), ( "tool--overlay", True ), ( "tool--visible", model.showingTools ) ]
                    "Add white"
                    ToRight
                    (NewTool AddWhite)
                    Big
                ]
            , li [ classList [ ( "list-item", True ) ] ]
                [ viewIconTextAction
                    "remove"
                    [ ( "tool", True ), ( "tool--overlay", True ), ( "tool--visible", model.showingTools ) ]
                    "Remove"
                    ToRight
                    (NewTool Remove)
                    Big
                ]
            ]
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
        position =
            ( y, x )

        stone =
            Dict.get position model.board
                |> Maybe.withDefault (Stone False Empty)

        isStar =
            case model.boardSize of
                Nineteen ->
                    isPositionStar model.stars19x19 x y

                Thirteen ->
                    isPositionStar model.stars13x13 x y

                Nine ->
                    isPositionStar model.stars9x9 x y

        isDead =
            Dict.get position model.dead
                |> Maybe.withDefault (Dead False Empty)
                |> .isDead

        territory =
            Dict.get position model.territory
                |> Maybe.withDefault (Territory False Empty)

        classColorStone =
            case stone.color of
                Black ->
                    "board-stone--black"

                White ->
                    "board-stone--white"

                Conflict ->
                    "board-stone--conflict"

                Empty ->
                    "board-stone--empty"

        classColorTerritory =
            if territory.isTerritory && model.state == Scoring then
                case territory.color of
                    Black ->
                        "board-point--territory-black"

                    White ->
                        "board-point--territory-white"

                    _ ->
                        "board-point--territory-neutral"
            else
                "board-point--territory-empty"
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
                    , ( "board-stone--dead", isDead )
                    , ( "board-stone--dead--editing", isDead && model.state == Editing )
                    , ( classColorStone, True )
                    ]
                , onClick (UseToolAt ( y, x ))
                ]
                []
            , span
                [ classList
                    [ ( "board-point", True )
                    , ( "board-point--star", isStar )
                    , ( classColorTerritory, True )
                    ]
                ]
                []
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
