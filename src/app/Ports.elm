port module Ports exposing (..)

import Array exposing (Array)


-- MODEL


type alias Probability =
    { probabilityStone : Int, probabilityBlack : Int, probabilityWhite : Int }



-- COMMAND


port useFile : String -> Cmd msg


port startCamera : Bool -> Cmd msg


port stopCamera : Bool -> Cmd msg


port takePhoto : Bool -> Cmd msg


port startCropping : Bool -> Cmd msg


port cropPhoto : Bool -> Cmd msg


port startPickingBlack : Bool -> Cmd msg


port startPickingWhite : Bool -> Cmd msg


port pickBlack : Bool -> Cmd msg


port pickWhite : Bool -> Cmd msg


port startProcessing : Bool -> Cmd msg



-- SUBSCRIPTION


port cameraStarted : (Bool -> msg) -> Sub msg


port cameraStopped : (Bool -> msg) -> Sub msg


port inputSuccessful : (Bool -> msg) -> Sub msg


port croppingSuccessful : (Bool -> msg) -> Sub msg


port pickingBlackSuccessful : (Bool -> msg) -> Sub msg


port pickingWhiteSuccessful : (Bool -> msg) -> Sub msg


port processingSuccessful : (Array Probability -> msg) -> Sub msg
