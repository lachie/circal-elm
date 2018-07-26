module View exposing (..)

import Html exposing (..)
import Html.Attributes as HAttrs
import View.Year exposing (yearView)
import Types exposing (..)


-- import ElementRelativeMouseEvents exposing (onMouseMove)


yearViewConfig =
    { diameter = 600
    , toMsg = SetYearViewState
    }


rootView : Model -> Html Msg
rootView { year, yearViewState } =
    div [ HAttrs.id "app-root" ]
        [ h1 [] [ text <| toString year.year ]
        , yearView yearViewConfig yearViewState year
        ]
