port module Ports exposing (..)


type alias FileSubscriptionPort =
    { key : String
    , id : String
    }


type alias FileDataPort =
    { key : String
    , name : String
    , content : String
    }


port fileSelected : FileSubscriptionPort -> Cmd msg


port fileContentRead : (FileDataPort -> msg) -> Sub msg
