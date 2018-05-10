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
    , showingDemo : Bool
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
    = MarkDead
    | MarkAlive
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
            , showingDemo = False
            , boardSizes = [ Nineteen, Thirteen, Nine ]
            , boardSize = Nineteen
            , komi = 6.5
            , prisonersBlack = 0
            , prisonersWhite = 0
            , areDeadRemoved = False
            , showHelp = False
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
            , showingTools = True
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


adSenseURL : String
adSenseURL =
    "//pagead2.googlesyndication.com/pagead/js/adsbygoogle.js"


adSenseScript : String
adSenseScript =
    "(adsbygoogle = window.adsbygoogle || []).push({google_ad_client: \"ca-pub-9243137751192540\",enable_page_level_ads: true});"



-- CALCULATION


neighboursOfPositionPerpendicular : BoardPosition -> List BoardPosition
neighboursOfPositionPerpendicular position =
    let
        y =
            Tuple.first position

        x =
            Tuple.second position
    in
        [ ( (Basics.max 1 (y - 1)), x )
        , ( y, (Basics.min 19 (x + 1)) )
        , ( (Basics.min 19 (y + 1)), x )
        , ( y, (Basics.max 1 (x - 1)) )
        ]


neighboursOfPositionDiagonal : BoardPosition -> List BoardPosition
neighboursOfPositionDiagonal position =
    let
        y =
            Tuple.first position

        x =
            Tuple.second position
    in
        [ ( (Basics.max 1 (y - 1)), (Basics.min 19 (x + 1)) )
        , ( (Basics.min 19 (y + 1)), (Basics.min 19 (x + 1)) )
        , ( (Basics.min 19 (y + 1)), (Basics.max 1 (x - 1)) )
        , ( (Basics.max 1 (y - 1)), (Basics.max 1 (x - 1)) )
        ]


positionToStonePosition : Board -> BoardPosition -> ( BoardPosition, Stone )
positionToStonePosition board position =
    Dict.get position board |> Maybe.withDefault (Stone False Empty) |> (,) position


isColorAndPositions : StoneColor -> BoardPosition -> BoardPosition -> ( BoardPosition, Stone ) -> Bool
isColorAndPositions color position1 position2 stone =
    let
        stonePosition =
            Tuple.first stone

        stoneColor =
            (Tuple.second stone).color
    in
        ((stonePosition == position1) || (stonePosition == position2)) && stoneColor == color


isNeighbourDiagonal : List ( BoardPosition, Stone ) -> StoneColor -> Int -> Int -> ( BoardPosition, Stone ) -> Bool
isNeighbourDiagonal stonesPerpendicular color xOrigin yOrigin stone =
    let
        position =
            Tuple.first stone

        y =
            Tuple.first position

        x =
            Tuple.second position

        colorNot =
            case color of
                Black ->
                    White

                White ->
                    Black

                _ ->
                    Empty
    in
        case ( y - yOrigin, x - xOrigin ) of
            ( -1, 1 ) ->
                -- NE
                let
                    stonesNot =
                        List.filter (isColorAndPositions colorNot ( yOrigin - 1, xOrigin ) ( yOrigin, xOrigin + 1 )) stonesPerpendicular
                in
                    List.length stonesNot < 2

            ( 1, 1 ) ->
                -- SE
                let
                    stonesNot =
                        List.filter (isColorAndPositions colorNot ( yOrigin, xOrigin + 1 ) ( yOrigin + 1, xOrigin )) stonesPerpendicular
                in
                    List.length stonesNot < 2

            ( 1, -1 ) ->
                -- SW
                let
                    stonesNot =
                        List.filter (isColorAndPositions colorNot ( yOrigin + 1, xOrigin ) ( yOrigin, xOrigin - 1 )) stonesPerpendicular
                in
                    List.length stonesNot < 2

            ( -1, -1 ) ->
                -- NW
                let
                    stonesNot =
                        List.filter (isColorAndPositions colorNot ( yOrigin, xOrigin - 1 ) ( yOrigin - 1, xOrigin )) stonesPerpendicular
                in
                    List.length stonesNot < 2

            _ ->
                False


findNeighboursForPosition : Board -> StoneColor -> BoardPosition -> List BoardPosition
findNeighboursForPosition board color position =
    let
        y =
            Tuple.first position

        x =
            Tuple.second position

        positionsPerpendicular =
            neighboursOfPositionPerpendicular position

        positionsDiagonal =
            neighboursOfPositionDiagonal position

        stonesPerpendicular =
            List.map (positionToStonePosition board) positionsPerpendicular

        stonesDiagonal =
            List.map (positionToStonePosition board) positionsDiagonal

        neighboursPerpendicular =
            List.filter (\n -> (Tuple.second n).color == color) stonesPerpendicular
                |> List.map (\n -> Tuple.first n)

        neighboursDiagonal =
            List.filter (\n -> (Tuple.second n).color == color) stonesDiagonal
                |> List.filter (isNeighbourDiagonal stonesPerpendicular color x y)
                |> List.map (\n -> Tuple.first n)
    in
        neighboursPerpendicular ++ neighboursDiagonal


findGroup : Board -> StoneColor -> List BoardPosition -> List BoardPosition -> List BoardPosition
findGroup board color positions group =
    case color of
        Empty ->
            []

        _ ->
            let
                neighbours =
                    List.map (findNeighboursForPosition board color) positions
                        |> List.concat

                newGroup =
                    List.filter (\n -> List.member n group |> not) neighbours
                        |> (++) group

                nextPositions =
                    List.filter (\p -> List.member p group |> not) neighbours
            in
                if List.length nextPositions == 0 then
                    newGroup
                else
                    findGroup board color nextPositions newGroup


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
        else
            Territory True Empty


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
    | ShowDemo
    | CloseDemo
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
    | NoMsg


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

        ShowDemo ->
            ( { model | showingDemo = True }, Cmd.none )

        CloseDemo ->
            ( { model | showingDemo = False }, Cmd.none )

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
            let
                state =
                    if removed then
                        Scoring
                    else
                        Editing
            in
                ( { model
                    | areDeadRemoved = removed
                    , state = state
                    , showingScore = removed
                    , showingTools = not removed
                  }
                , Cmd.none
                )

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

                territory =
                    findTerritories board model.dead

                scoreBlack =
                    calculateScoreBlack territory model.dead model.prisonersBlack

                scoreWhite =
                    calculateScoreWhite territory model.dead model.prisonersWhite model.komi
            in
                ( { model
                    | isProcessingSuccessful = True
                    , board = board
                    , territory = territory
                    , scoreBlack = scoreBlack
                    , scoreWhite = scoreWhite
                  }
                , Cmd.none
                )

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

                color =
                    Dict.get position model.board
                        |> Maybe.withDefault (Stone False Empty)
                        |> .color

                dead =
                    case model.tool of
                        MarkDead ->
                            let
                                group =
                                    findGroup board color [ position ] [ position ]

                                newDead =
                                    List.map (\p -> ( p, (Dead True color) )) group
                                        |> Dict.fromList
                            in
                                Dict.union newDead model.dead

                        MarkAlive ->
                            let
                                group =
                                    findGroup board color [ position ] [ position ]

                                newAlive =
                                    List.map (\p -> ( p, (Dead False color) )) group
                                        |> Dict.fromList
                            in
                                Dict.diff model.dead newAlive

                        _ ->
                            model.dead
            in
                ( { model | board = board, dead = dead }, Cmd.none )

        NoMsg ->
            ( model, Cmd.none )



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
                [ span [ classList [ ( "loading-stone", True ), ( "loading-stone--black", True ) ] ] []
                , span [ classList [ ( "loading-stone", True ), ( "loading-stone--white", True ) ] ] []
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
        div [ classList classes ]
            content


viewIconTextCommand : String -> List ( String, Bool ) -> String -> TextDirection -> Msg -> ViewSize -> Html Msg
viewIconTextCommand name classesExtra value direction msg size =
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
        div [ classList classes, onClick msg ]
            content


viewIconTextAction : String -> List ( String, Bool ) -> String -> TextDirection -> Msg -> ViewSize -> Html Msg
viewIconTextAction name classesExtra value direction msg size =
    let
        classes =
            [ ( "action", True ) ] ++ classesExtra
    in
        viewIconTextCommand name classes value direction msg size


viewIconTextActionClose : Html Msg
viewIconTextActionClose =
    viewIconTextAction "close" [ ( "action--close", True ) ] "Close" ToLeft (ChangePath "#") Big


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
        div [ classList [ ( "message", True ), ( "message--visible", isVisible ) ] ]
            [ div [ classList [ ( "message-background", True ) ] ] []
            , ul [ classList [ ( "message-list", True ) ] ]
                ([ viewIconTextAction
                    "close"
                    [ ( "action--close", True ) ]
                    "Close"
                    ToLeft
                    (CloseMessage name message.content)
                    Medium
                 ]
                    ++ (List.map viewMessagePart message.content)
                )
            ]



-- VIEW DRAWING


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
                    , div [ id (identifier ++ "-zoom"), classList [ ( "crop-frame-zoom", True ) ] ]
                        [ div [ id (identifier ++ "-zoom-in"), classList [ ( "crop-frame-zoom--in", True ) ] ]
                            [ viewIcon "remove" [ ( "icon--big", True ) ] ]
                        , div
                            [ id (identifier ++ "-zoom-percentage")
                            , classList [ ( "crop-frame-zoom--percentage", True ) ]
                            ]
                            []
                        , div [ id (identifier ++ "-zoom-out"), classList [ ( "crop-frame-zoom--out", True ) ] ]
                            [ viewIcon "add" [ ( "icon--big", True ) ] ]
                        ]
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



-- VIEW CONTENT


viewLanding : Model -> Html Msg
viewLanding model =
    div [ classList [ ( "container-landing", True ) ] ]
        [ ul [ classList [ ( "landing-list", True ) ] ]
            [ li [ classList [ ( "landing-list-item", True ), ( "list-item", True ) ] ]
                [ viewIconTextAction
                    "start"
                    []
                    "Start"
                    ToRight
                    (ChangePath "#settings")
                    Big
                ]
            , li [ classList [ ( "landing-list-item", True ), ( "list-item", True ) ] ]
                [ viewIconTextAction
                    "video"
                    []
                    "Demo"
                    ToRight
                    ShowDemo
                    Big
                ]
            ]
        , div [ classList [ ( "container-landing-pitch", True ) ] ]
            [ div [ classList [ ( "landing-logo", True ) ] ]
                [ viewIcon "logo" [ ( "icon--logo", True ) ]
                ]
            , p [ classList [ ( "landing-blurb", True ) ] ]
                [ text "Automatically score your finished Go/Baduk/Weiqi games in just a few simple steps" ]
            ]
        , viewDemo model
        ]


viewDemo : Model -> Html Msg
viewDemo model =
    case model.showingDemo of
        True ->
            div [ classList [ ( "demo", True ), ( "demo--visible", True ) ] ]
                [ div [ classList [ ( "demo-background", True ) ] ] []
                , viewIconTextAction
                    "close"
                    [ ( "action--close", True ) ]
                    "Close"
                    ToLeft
                    CloseDemo
                    Medium
                , div [ classList [ ( "demo-video", True ), ( "demo-video--loading", True ) ] ] 
                    [ div [ classList [ ( "loading-animation", True ) ] ]
                        [ span [ classList [ ( "loading-stone", True ), ( "loading-stone--black", True ) ] ] []
                        , span [ classList [ ( "loading-stone", True ), ( "loading-stone--white", True ) ] ] []
                        ]
                    ]
                , iframe
                    [ classList [ ( "demo-video", True ) ]
                    , src "https://www.youtube-nocookie.com/embed/T-rPyxODzCk?rel=0"
                    , attributeFrameBorder "0"
                    , attributeAllow "autoplay; encrypted-media"
                    , attributeAllowFullScreen
                    ]
                    []
                ]

        False ->
            div [ classList [ ( "demo", True ) ] ] []


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
            [ viewIconTextActionClose
            , viewIconTextAction
                "continue"
                [ ( "action--thumb", True ) ]
                "Continue"
                ToRight
                (ChangePath "#source")
                Big
            , div [ classList [ ( "container-settings-list", True ) ] ]
                [ ul [ classList [ ( "settings-list", True ) ] ]
                    [ li [ classList [ ( "list-item", True ) ] ]
                        [ label [ for "prisoners-black" ]
                            [ viewIconText "white" [] "Captured by black" ToRight False Medium
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
                            [ viewIconText "black" [] "Captured by white" ToRight False Medium
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
                    , li [ classList [ ( "list-item", True ) ] ]
                        [ label [ for "komi" ]
                            [ viewIconText "komi" [] "Komi" ToRight False Medium
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
                    ]
                , ul [ classList [ ( "settings-list", True ) ] ]
                    [ li [ classList [ ( "list-item", True ) ] ]
                        [ viewIconTextCommand
                            nameAreDeadRemoved
                            []
                            "Dead stones have been removed"
                            ToRight
                            (NewAreDeadRemoved <| not model.areDeadRemoved)
                            Medium
                        ]
                    , li [ classList [ ( "list-item", True ) ] ]
                        [ viewIconTextCommand
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
        [ viewIconTextActionClose
        , ul []
            [ li [ classList [ ( "container-link", True ), ( "list-item", True ) ] ]
                [ viewIconTextAction "camera" [] "Photo" ToRight (ChangePath "#photo") Big ]
            , li [ classList [ ( "container-input", True ), ( "list-item", True ) ] ]
                [ label [ for "file-input" ]
                    [ viewIconTextAction "file" [] "File" ToRight NoMsg Big
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
                    [ viewIconTextActionClose
                    , viewMessage "photo" True model.messages model.showHelp
                    ]

                False ->
                    [ viewIconTextActionClose
                    , viewIconTextAction
                        "camera"
                        [ ( "action--center", True ) ]
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
                    [ viewIconTextActionClose
                    , viewMessage "crop" True model.messages model.showHelp
                    , viewIconTextAction
                        "crop"
                        [ ( "action--thumb", True ) ]
                        "Crop"
                        ToRight
                        CropPhoto
                        Big
                    ]

                False ->
                    [ viewIconTextActionClose
                    , viewIconText
                        "info"
                        [ ( "info", True ) ]
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
                    [ viewIconTextActionClose
                    , viewMessage "black" True model.messages model.showHelp
                    , viewIconTextAction
                        "color"
                        [ ( "action--thumb", True ) ]
                        "Pick"
                        ToRight
                        PickBlack
                        Big
                    ]

                False ->
                    [ viewIconTextActionClose
                    , viewIconText
                        "info"
                        [ ( "info", True ) ]
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
                    [ viewIconTextActionClose
                    , viewMessage "white" True model.messages model.showHelp
                    , viewIconTextAction
                        "color"
                        [ ( "action--thumb", True ) ]
                        "Pick"
                        ToRight
                        PickWhite
                        Big
                    ]

                False ->
                    [ viewIconTextActionClose
                    , viewIconText
                        "info"
                        [ ( "info", True ) ]
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
                    [ viewIconTextActionClose
                    , viewMessage "processing" True model.messages model.showHelp
                    , viewIconTextAction
                        "continue"
                        [ ( "action--thumb", True ) ]
                        "Continue"
                        ToRight
                        (ChangePath "#score")
                        Big
                    ]

                False ->
                    [ viewIconTextActionClose
                    , viewIconText
                        "info"
                        [ ( "info", True ) ]
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
        [ viewIconTextActionClose
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
                [ viewIconTextAction
                    "score"
                    [ ( "action--active", model.state == Scoring ) ]
                    "Score"
                    ToRight
                    (NewState Scoring)
                    Big
                ]
            , li [ classList [ ( "list-item", True ) ] ]
                [ viewIconTextAction
                    "tools"
                    [ ( "action--active", model.state == Editing ) ]
                    "Tools"
                    ToRight
                    (NewState Editing)
                    Big
                ]
            ]
        , ul [ classList [ ( "controls-score", True ), ( "controls-score--visible", model.showingScore ) ] ]
            [ li [ classList [ ( "list-item", True ) ] ]
                [ viewIconText
                    "black"
                    []
                    (toString model.scoreBlack)
                    ToRight
                    False
                    Big
                ]
            , li [ classList [ ( "list-item", True ) ] ]
                [ viewIconText
                    "white"
                    []
                    (toString model.scoreWhite)
                    ToRight
                    False
                    Big
                ]
            ]
        , ul [ classList [ ( "controls-tools", True ), ( "controls-tools--visible", model.showingTools ) ] ]
            [ li [ classList [ ( "list-item", True ) ] ]
                [ viewIconTextAction
                    "deadblack"
                    []
                    "Dead"
                    ToRight
                    (NewTool MarkDead)
                    Big
                ]
            , li [ classList [ ( "list-item", True ) ] ]
                [ viewIconTextAction
                    "deadwhite"
                    []
                    "Alive"
                    ToRight
                    (NewTool MarkAlive)
                    Big
                ]
            , li [ classList [ ( "list-item", True ) ] ]
                [ viewIconTextAction
                    "addblack"
                    []
                    "Add black"
                    ToRight
                    (NewTool AddBlack)
                    Big
                ]
            , li [ classList [ ( "list-item", True ) ] ]
                [ viewIconTextAction
                    "addwhite"
                    []
                    "Add white"
                    ToRight
                    (NewTool AddWhite)
                    Big
                ]
            , li [ classList [ ( "list-item", True ) ] ]
                [ viewIconTextAction
                    "remove"
                    []
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


attributeFrameBorder : String -> Attribute Msg
attributeFrameBorder value =
    attribute "frameborder" value


attributeAllow : String -> Attribute Msg
attributeAllow value =
    attribute "allow" value


attributeAllowFullScreen : Attribute Msg
attributeAllowFullScreen =
    attribute "allowfullscreen" ""


attributeAsync : Attribute Msg
attributeAsync =
    attribute "async" ""


script : List (Attribute Msg) -> List (Html Msg) -> Html Msg
script attributes children =
    node "script" attributes children
