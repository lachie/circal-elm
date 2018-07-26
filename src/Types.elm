module Types exposing (..)

import Mouse
import Types.Year exposing (Year, Day)
import View.Year exposing (YearViewState)


type Msg
    = SelectDay Day
    | SetYearViewState YearViewState
    | YearAngle Float
      -- | YearMove DaySelection
    | DragStart Mouse.Position
    | DragAt Mouse.Position
    | DragEnd Mouse.Position


type alias Event =
    { title : String }


type alias Model =
    { year : Year
    , yearViewState : YearViewState
    }
