port module Ports exposing (..)

import Array exposing (Array)


type alias ImageData =
    { dataArray : Array Int, dataBase64 : String, width : Int, height : Int }


port fileSelected : String -> Cmd msg


port startCamera : Bool -> Cmd msg


port stopCamera : Bool -> Cmd msg


port takePhoto : Bool -> Cmd msg


port processImage : (ImageData -> msg) -> Sub msg
