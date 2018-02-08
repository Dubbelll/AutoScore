port module Ports exposing (..)


type alias FileDataPort =
    { name : String
    , content : String
    }


port fileSelected : String -> Cmd msg


port fileContentRead : (FileDataPort -> msg) -> Sub msg
