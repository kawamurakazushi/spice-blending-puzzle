module MainTests exposing (suite)

import Expect exposing (Expectation)
import Main
import Test exposing (..)


board : Main.Board
board =
    List.range 1 4
        |> List.map
            (\y ->
                List.range 1 4
                    |> List.map
                        (\x -> Main.Cell (Main.Point x y) Main.Blank)
            )


suite : Test
suite =
    describe "methods"
        [ describe "range"
            [ test "range 0 1" <|
                \_ -> Main.range 0 1 |> Expect.equal [ 0, 1 ]
            , test "range 0 -1" <|
                \_ -> Main.range 0 -1 |> Expect.equal [ -1, 0 ]
            ]
        , describe "fill"
            [ test "can fill normally (1,1) -> (2,2)" <|
                \_ ->
                    let
                        ans =
                            board
                                |> Main.set (Main.Point 1 1) Main.Selected
                                |> Main.set (Main.Point 1 2) Main.Selected
                                |> Main.set (Main.Point 2 1) Main.Selected
                                |> Main.set (Main.Point 2 2) Main.Selected
                    in
                    board
                        |> Main.fill (Main.Point 1 1) (Main.Point 2 2)
                        |> Expect.equal ans
            , test "can fill normally (2,2) -> (1,1)" <|
                \_ ->
                    let
                        ans =
                            board
                                |> Main.set (Main.Point 1 1) Main.Selected
                                |> Main.set (Main.Point 1 2) Main.Selected
                                |> Main.set (Main.Point 2 1) Main.Selected
                                |> Main.set (Main.Point 2 2) Main.Selected
                    in
                    board
                        |> Main.fill (Main.Point 2 2) (Main.Point 1 1)
                        |> Expect.equal ans
            , test "can fill normally (2,2) -> (3,1)" <|
                \_ ->
                    let
                        ans =
                            board
                                |> Main.set (Main.Point 2 1) Main.Selected
                                |> Main.set (Main.Point 2 2) Main.Selected
                                |> Main.set (Main.Point 3 1) Main.Selected
                                |> Main.set (Main.Point 3 2) Main.Selected
                    in
                    board
                        |> Main.fill (Main.Point 2 2) (Main.Point 3 1)
                        |> Expect.equal ans
            , test "can fill normally (2,2) -> (1,3)" <|
                \_ ->
                    let
                        ans =
                            board
                                |> Main.set (Main.Point 2 2) Main.Selected
                                |> Main.set (Main.Point 2 3) Main.Selected
                                |> Main.set (Main.Point 1 2) Main.Selected
                                |> Main.set (Main.Point 1 3) Main.Selected
                    in
                    board
                        |> Main.fill (Main.Point 2 2) (Main.Point 1 3)
                        |> Expect.equal ans
            , test "can fill normally (1,1) -> (2,3)" <|
                \_ ->
                    let
                        ans =
                            board
                                |> Main.set (Main.Point 1 1) Main.Selected
                                |> Main.set (Main.Point 1 2) Main.Selected
                                |> Main.set (Main.Point 1 3) Main.Selected
                                |> Main.set (Main.Point 1 4) Main.Selected
                                |> Main.set (Main.Point 2 1) Main.Selected
                                |> Main.set (Main.Point 2 2) Main.Selected
                                |> Main.set (Main.Point 2 3) Main.Selected
                                |> Main.set (Main.Point 2 4) Main.Selected
                    in
                    board
                        |> Main.fill (Main.Point 1 1) (Main.Point 2 3)
                        |> Expect.equal ans
            , test "can fill normally (2,4) -> (1,2)" <|
                \_ ->
                    let
                        ans =
                            board
                                |> Main.set (Main.Point 1 1) Main.Selected
                                |> Main.set (Main.Point 1 2) Main.Selected
                                |> Main.set (Main.Point 1 3) Main.Selected
                                |> Main.set (Main.Point 1 4) Main.Selected
                                |> Main.set (Main.Point 2 1) Main.Selected
                                |> Main.set (Main.Point 2 2) Main.Selected
                                |> Main.set (Main.Point 2 3) Main.Selected
                                |> Main.set (Main.Point 2 4) Main.Selected
                    in
                    board
                        |> Main.fill (Main.Point 2 4) (Main.Point 1 2)
                        |> Expect.equal ans
            , test "can fill normally (1,2) -> (2,4)" <|
                \_ ->
                    let
                        ans =
                            board
                                |> Main.set (Main.Point 1 3) Main.Selected
                                |> Main.set (Main.Point 1 2) Main.Selected
                                |> Main.set (Main.Point 2 2) Main.Selected
                                |> Main.set (Main.Point 2 3) Main.Selected
                    in
                    board
                        |> Main.fill (Main.Point 1 2) (Main.Point 2 4)
                        |> Expect.equal ans
            ]
        ]
