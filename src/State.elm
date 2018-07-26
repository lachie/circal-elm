module State exposing (..)

import Date exposing (Date)
import Types exposing (..)
import Types.Year exposing (yearForMilliseconds)
import View
import View.Year
import Mouse exposing (Position)


type alias Flags =
    { initialMilliseconds : Float
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        year =
            yearForMilliseconds flags.initialMilliseconds

        ( yearViewState, yearViewCmd ) =
            View.Year.initYearState
    in
        (Model year yearViewState) ! [ Cmd.map SetYearViewState yearViewCmd ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            Debug.log "msg" msg
    in
        case msg of
            SetYearViewState newState ->
                ( { model | yearViewState = newState }, Cmd.none )

            -- SelectDay day ->
            -- ( { model | selectedDay = Just day }, Cmd.none )
            -- YearMove { i } ->
            -- ( (updateYearMove model i), Cmd.none )
            _ ->
                ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- subscriptions : Model -> Sub Msg
-- subscriptions model =
-- Sub.batch
-- [ Mouse.moves MouseAt
-- , Mouse.ups DragEnd
-- ]
