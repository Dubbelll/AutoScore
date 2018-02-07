module TestApp exposing (..)

import App
import Expect exposing (Expectation)
import Test exposing (..)


suite : Test
suite =
    describe "The App module"
        [ describe "fullURL"
            [ test "valid config results in valid URL" <|
                \_ ->
                    let
                        config =
                            App.Config "v1" "http://localhost:3000/api"
                    in
                        Expect.equal config.baseURL "http://localhost:3000/api"
            ]
        ]
