module DateTest exposing (suite)

import Date
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Time


fixtures =
    { jdn20191010 = 2458767
    , jdn20211010 = 2459498
    }


suite : Test
suite =
    describe "Date module"
        [ describe "julianDayNumber"
            [ test "works" <|
                \_ -> Date.julianDayNumber 2019 10 10 |> Expect.equal fixtures.jdn20191010
            ]
        , describe "fromJulianDayNumber"
            [ test "works" <|
                \_ -> Date.fromJulianDayNumber fixtures.jdn20191010 |> Expect.equal ( 2019, Time.Oct, 10 )
            ]
        , describe "addYears"
            [ test "works" <|
                \_ -> Date.addYears fixtures.jdn20191010 2 |> Expect.equal fixtures.jdn20211010
            ]
        ]
