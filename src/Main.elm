module Main exposing (main)

import Browser
import Html


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
    ( {}, Cmd.none )


view : Model -> Browser.Document Msg
view _ =
    { title = "Hello, world!"
    , body = [ Html.text "Hello, world!" ]
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update _ model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


type alias Flags =
    ()


type alias Model =
    {}


type alias Msg =
    Never
