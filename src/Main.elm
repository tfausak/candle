module Main exposing (main)

import Browser
import Html
import Html.Attributes as Attr
import Task
import Time


main : Program Flags Model Msg
main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Flags -> ( Model, Cmd Msg )
init _ =
    ( { time = Nothing
      , zone = Nothing
      }
    , Cmd.batch
        [ Task.perform NewZone Time.here
        , Task.perform NewTime Time.now
        ]
    )


view : Model -> Browser.Document Msg
view model =
    { title = "Candle"
    , body =
        case ( model.time, model.zone ) of
            ( Just time, Just zone ) ->
                [ Html.div
                    [ Attr.style "font-family" "monospace"
                    , Attr.style "text-align" "center"
                    ]
                    [ Html.text <|
                        String.concat
                            [ formatYear <| Time.toYear zone time
                            , "-"
                            , formatMonth <| Time.toMonth zone time
                            , "-"
                            , formatDay <| Time.toDay zone time
                            , " "
                            , formatHour <| Time.toHour zone time
                            , ":"
                            , formatMinute <| Time.toMinute zone time
                            , ":"
                            , formatSecond <| Time.toSecond zone time
                            ]
                    ]
                ]

            _ ->
                []
    }


formatYear : Int -> String
formatYear =
    String.fromInt


formatMonth : Time.Month -> String
formatMonth month =
    case month of
        Time.Jan ->
            "1"

        Time.Feb ->
            "2"

        Time.Mar ->
            "3"

        Time.Apr ->
            "4"

        Time.May ->
            "5"

        Time.Jun ->
            "6"

        Time.Jul ->
            "7"

        Time.Aug ->
            "8"

        Time.Sep ->
            "9"

        Time.Oct ->
            "10"

        Time.Nov ->
            "11"

        Time.Dec ->
            "12"


formatDay : Int -> String
formatDay day =
    if day < 10 then
        "0" ++ String.fromInt day

    else
        String.fromInt day


formatHour : Int -> String
formatHour =
    formatDay


formatMinute : Int -> String
formatMinute =
    formatDay


formatSecond : Int -> String
formatSecond =
    formatDay


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewTime time ->
            ( { model | time = Just time }, Cmd.none )

        NewZone zone ->
            ( { model | zone = Just zone }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 1000 NewTime


type alias Flags =
    ()


type alias Model =
    { time : Maybe Time.Posix
    , zone : Maybe Time.Zone
    }


type Msg
    = NewTime Time.Posix
    | NewZone Time.Zone
