port module Ports exposing (..)

import Array exposing (Array)


type alias ImageData =
    { dataArray : Array Int, dataBase64 : String, width : Int, height : Int }


port imageSelected : String -> Cmd msg


port processImage : (ImageData -> msg) -> Sub msg
