port module Ports exposing (..)

import Array exposing (Array)


-- MODEL


type alias WindowSizeData =
    { width : Float, height : Float }


type alias ImageData =
    { dataArray : Array Int, dataBase64 : String, width : Int, height : Int }


type alias CropData =
    { id : String, x : Int, y : Int, width : Float, height : Float }


type alias PixelsData =
    Array Int


type alias CanvasPixelsData =
    { width : Float, height : Float, pixels : PixelsData }



-- COMMAND


port takeWindowSizeSnapshot : Bool -> Cmd msg


port fileSelected : String -> Cmd msg


port startCamera : Bool -> Cmd msg


port stopCamera : Bool -> Cmd msg


port takePhoto : Bool -> Cmd msg


port cropPhoto : CropData -> Cmd msg


port drawPixels : CanvasPixelsData -> Cmd msg



-- SUBSCRIPTION


port windowSizeSnapshotted : (WindowSizeData -> msg) -> Sub msg


port cameraStarted : (Bool -> msg) -> Sub msg


port cameraStopped : (Bool -> msg) -> Sub msg


port imageProcessed : (ImageData -> msg) -> Sub msg


port pixelsProcessed : (PixelsData -> msg) -> Sub msg
