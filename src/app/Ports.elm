port module Ports exposing (..)


type alias ImageData =
    { dataArray : List Int, dataBase64 : String, width : Int, height : Int }


port imageSelected : String -> Cmd msg


port processImage : (ImageData -> msg) -> Sub msg
