module Main exposing (main)

import Html
import Query


main : Program Never (Query.Model Int) (Query.Msg Int msg)
main =
    Query.program
        { webSocketUrl = "ws://localhost:8000"
        , init = ( 0, Query.int, Cmd.none )
        , update = (\_ model -> ( model, Query.int, Cmd.none ))
        , subscriptions = (\_ -> Sub.none)
        , view = toString >> Html.text >> List.singleton >> Html.div []
        }
