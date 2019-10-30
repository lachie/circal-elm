module Tetris exposing (Tetris(..), dropIn, empty)


type Tetris a
    = Tetris (List (List a))


empty : Tetris a
empty =
    Tetris []


dropIn : Tetris a -> (List a -> a -> Bool) -> a -> Tetris a
dropIn (Tetris l) canPush i =
    Tetris (dropInHelp l canPush i)


dropInHelp : List (List a) -> (List a -> a -> Bool) -> a -> List (List a)
dropInHelp l canPush i =
    case l of
        [] ->
            [ [ i ] ]

        row :: rest ->
            if canPush row i then
                (row ++ [ i ]) :: rest

            else
                row :: dropInHelp rest canPush i
