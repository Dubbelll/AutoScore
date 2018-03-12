port module Ports exposing (..)

import Array exposing (Array)


type alias ImageData =
    { dataArray : Array Int, dataBase64 : String, width : Int, height : Int }


type alias ElementSizeData =
    { width : Float, height : Float }


port fileSelected : String -> Cmd msg


port startCamera : Bool -> Cmd msg


port stopCamera : Bool -> Cmd msg


port takePhoto : Bool -> Cmd msg


port cropPhoto : Bool -> Cmd msg


port takeElementSizeSnapshot : String -> Cmd msg


port processImage : (ImageData -> msg) -> Sub msg


port snapshotElementSize : (ElementSizeData -> msg) -> Sub msg
