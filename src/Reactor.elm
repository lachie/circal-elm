module Reactor exposing (..)

import Html exposing (Html)
import State
import View
import Types exposing (Model, Msg)


-- main : Program Never Model Msg
main =
  Html.program
        { init = State.init { initialMilliseconds = 1531890186338 }
        , update = State.update
        , subscriptions = State.subscriptions
        , view = View.rootView
        }
