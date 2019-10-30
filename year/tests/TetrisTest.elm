module TetrisTest exposing (suite)

import Expect exposing (Expectation)
import Test exposing (..)
import Tetris


type alias Rec =
    ( Int, Int )


extendRec : Rec -> Rec -> Rec
extendRec l r =
    let
        ( l1, r1 ) =
            l

        ( l2, r2 ) =
            r
    in
    ( min l1 l2, max r1 r2 )


maxInt =
    2 ^ 31 - 1


overlap : List Rec -> Rec -> Bool
overlap l r =
    let
        ( _, right ) =
            List.foldl extendRec ( maxInt, 0 ) l

        ( left, _ ) =
            r
    in
    left > right


suite : Test
suite =
    let
        doverlap =
            \r t -> Tetris.dropIn t overlap r
    in
    describe "Tetris module"
        [ describe "dropIn"
            [ test "works" <|
                \_ ->
                    let
                        t =
                            Tetris.empty
                    in
                    t |> Expect.equal Tetris.empty
            , test "dropIn empty" <|
                \_ ->
                    Tetris.empty |> doverlap ( 0, 0 ) |> Expect.equal (Tetris.Tetris [ [ ( 0, 0 ) ] ])
            , test "dropIn one row" <|
                \_ ->
                    Tetris.empty |> doverlap ( 0, 1 ) |> doverlap ( 2, 3 ) |> Expect.equal (Tetris.Tetris [ [ ( 0, 1 ), ( 2, 3 ) ] ])
            , test "dropIn overlapping" <|
                \_ ->
                    Tetris.empty |> doverlap ( 0, 2 ) |> doverlap ( 1, 3 ) |> Expect.equal (Tetris.Tetris [ [ ( 0, 2 ) ], [ ( 1, 3 ) ] ])

            -- Tetris.fromList [ [ ( 0, 0 ) ] ]
            ]
        ]
