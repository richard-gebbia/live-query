port module Main exposing (main)

import Counter
import Json.Decode as Json
import Platform
import Query.Server as Server


-- Model


type alias Model =
    { counter : Counter.Model
    , server : Server.Model
    }


init : Model
init =
    { counter = Counter.init
    , server = Server.init
    }



-- Msg


type Msg
    = CounterMsg Counter.Msg
    | ServerMsg Server.Msg



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CounterMsg counterMsg ->
            let
                updatedCounter =
                    Counter.update counterMsg model.counter

                queryResponse =
                    Counter.queryResponse updatedCounter

                ( updatedServer, _, serverCmd ) =
                    Server.newState stateUpdates queryResponse model.server
            in
                { model
                    | counter = updatedCounter
                    , server = updatedServer
                }
                    ! [ Cmd.map ServerMsg serverCmd ]

        ServerMsg serverMsg ->
            let
                ( updatedServer, _, serverCmd ) =
                    Server.update stateUpdates serverMsg model.server
            in
                { model
                    | server = updatedServer
                }
                    ! [ Cmd.map ServerMsg serverCmd ]



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Sub.map CounterMsg (Counter.subscriptions model.counter)
        , Sub.map ServerMsg (Server.subscriptions clientMessages clientDisconnects model.server)
        ]



-- Incoming Ports


port clientMessages : (Json.Value -> msg) -> Sub msg


port clientDisconnects : (Int -> msg) -> Sub msg



-- Outgoing Ports


port stateUpdates : Server.StateUpdate -> Cmd msg



-- Main


main =
    Platform.program
        { init = ( init, Cmd.none )
        , update = update
        , subscriptions = subscriptions
        }
