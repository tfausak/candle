module Main exposing (main)

import Browser
import Dict
import Html
import Html.Attributes as Attr
import Http
import Json.Decode as Decode
import Process
import Task
import Time
import Url.Builder as Url


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
    let
        model =
            { notifications =
                { notifications = []
                , pollInterval = Nothing
                }
            , time = Nothing
            , zone = Nothing
            }
    in
    ( model
    , Cmd.batch
        [ Task.perform NewZone Time.here
        , Task.perform NewTime Time.now
        , getNotifications
        ]
    )


getNotifications : Cmd Msg
getNotifications =
    Http.send handleNotificationResult notificationRequest


handleNotificationResult : Result Http.Error (Maybe Notifications) -> Msg
handleNotificationResult result =
    case result of
        Err error ->
            NewError <| HttpError error

        Ok maybeNotifications ->
            case maybeNotifications of
                Nothing ->
                    Noop

                Just notifications ->
                    NewNotifications notifications


notificationRequest : Http.Request (Maybe Notifications)
notificationRequest =
    Http.request
        { method = "GET"
        , headers = [ Http.header "Accept" "application/vnd.github.v3+json" ]
        , url = notificationUrl
        , body = Http.emptyBody
        , expect = Http.expectStringResponse handleNotificationResponse
        , timeout = Nothing
        , withCredentials = False
        }


handleNotificationResponse :
    Http.Response String
    -> Result String (Maybe Notifications)
handleNotificationResponse response =
    if response.status.code == 304 then
        Ok Nothing

    else
        let
            pollInterval =
                response.headers
                    |> Dict.get "x-poll-interval"
                    |> Maybe.andThen String.toInt

            result =
                Decode.decodeString
                    (decodeNotifications pollInterval)
                    response.body
        in
        case result of
            Err error ->
                Err (Decode.errorToString error)

            Ok notifications ->
                Ok (Just notifications)


notificationUrl : String
notificationUrl =
    Url.crossOrigin "https://api.github.com"
        [ "notifications" ]
        [ Url.string "access_token" "TODO"
        , Url.string "all" "true"
        , Url.int "per_page" 10
        ]


type alias Notifications =
    { notifications : List Notification
    , pollInterval : Maybe Int
    }


decodeNotifications : Maybe Int -> Decode.Decoder Notifications
decodeNotifications pollInterval =
    Decode.map
        (\notifications ->
            { notifications = notifications
            , pollInterval = pollInterval
            }
        )
        (Decode.list decodeNotification)


type alias Notification =
    { owner : String
    , repository : String
    , title : String
    , type_ : String
    }


decodeNotification : Decode.Decoder Notification
decodeNotification =
    Decode.map4 Notification
        (Decode.at [ "repository", "owner", "login" ] Decode.string)
        (Decode.at [ "repository", "name" ] Decode.string)
        (Decode.at [ "subject", "title" ] Decode.string)
        (Decode.at [ "subject", "type" ] Decode.string)


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
                    [ Html.text (formatTime zone time) ]
                , renderNotifications model.notifications
                ]

            _ ->
                []
    }


renderNotifications : Notifications -> Html.Html Msg
renderNotifications notifications =
    Html.ul [] (List.map renderNotification notifications.notifications)


renderNotification : Notification -> Html.Html Msg
renderNotification notification =
    Html.li []
        [ Html.text <|
            String.concat
                [ notification.owner
                , "/"
                , notification.repository
                , ": "
                , notification.type_
                , ": "
                , notification.title
                ]
        ]


formatTime : Time.Zone -> Time.Posix -> String
formatTime zone time =
    String.concat
        [ formatYear (Time.toYear zone time)
        , "-"
        , formatMonth (Time.toMonth zone time)
        , "-"
        , formatDay (Time.toDay zone time)
        , " "
        , formatHour (Time.toHour zone time)
        , ":"
        , formatMinute (Time.toMinute zone time)
        , ":"
        , formatSecond (Time.toSecond zone time)
        ]


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

        GetNotifications ->
            ( model, getNotifications )

        NewNotifications notifications ->
            ( { model | notifications = notifications }
            , Process.sleep
                (notifications.pollInterval
                    |> Maybe.withDefault 60
                    |> toFloat
                    |> secondsToMilliseconds
                )
                |> Task.perform (always GetNotifications)
            )

        NewError _ ->
            ( model, Cmd.none )

        Noop ->
            ( model, Cmd.none )


secondsToMilliseconds : Float -> Float
secondsToMilliseconds seconds =
    seconds * 1000


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every (secondsToMilliseconds 1) NewTime


type alias Flags =
    ()


type alias Model =
    { notifications : Notifications
    , time : Maybe Time.Posix
    , zone : Maybe Time.Zone
    }


type Msg
    = NewTime Time.Posix
    | NewZone Time.Zone
    | GetNotifications
    | NewNotifications Notifications
    | NewError Error
    | Noop


type Error
    = HttpError Http.Error
