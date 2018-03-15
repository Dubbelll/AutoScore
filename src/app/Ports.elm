port module Ports exposing (..)

-- MODEL


type alias Detection =
    { color : String, height : Int, width : Int, x : Int, y : Int }


type alias Crop =
    { x : Int, y : Int, width : Int, height : Int }



-- COMMAND


port useFile : String -> Cmd msg


port startCamera : Bool -> Cmd msg


port stopCamera : Bool -> Cmd msg


port takePhoto : Bool -> Cmd msg


port startCropping : Bool -> Cmd msg


port cropPhoto : Bool -> Cmd msg


port startProcessing : Bool -> Cmd msg



-- SUBSCRIPTION


port cameraStarted : (Bool -> msg) -> Sub msg


port cameraStopped : (Bool -> msg) -> Sub msg


port inputSuccessful : (Bool -> msg) -> Sub msg


port croppingSuccessful : (Crop -> msg) -> Sub msg


port stoneDetected : (Detection -> msg) -> Sub msg


port processingSuccessful : (Bool -> msg) -> Sub msg
