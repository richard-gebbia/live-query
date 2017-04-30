module Query.Server exposing (ClientId, Model, init, Msg, update, newState, subscriptions, StateUpdate)

import IntDict exposing (IntDict)
import Internal.Types exposing (QueryExpression, QueryResponse, KnownValue(..), SelectError, QueryParseError)
import Json.Decode as Json
import Query exposing (SelectResult)


-- Model


type alias ClientId =
    Int


type alias QueryId =
    Int


type alias Model =
    { clientQueries : IntDict QueryExpression
    }


init : Model
init =
    { clientQueries = IntDict.empty
    }


type alias StateUpdate =
    List { clients : List ClientId, message : String }


type alias StateUpdateEvent =
    { stateUpdate : StateUpdate
    , errors : List ( ClientId, SelectError )
    }



-- Msg


type alias ClientMsg =
    { clientId : ClientId, message : String }


clientMsgDecoder : Json.Decoder ClientMsg
clientMsgDecoder =
    Json.map2 ClientMsg
        (Json.field "clientId" Json.int)
        (Json.field "message" Json.string)


type Msg
    = ClientMessage ClientMsg
    | ClientDisconnect ClientId
    | StateUpdate QueryResponse
    | NoOp



-- Update


type ServerEvent
    = QueryParseFail ClientId QueryParseError
    | SelectFail (List ( ClientId, SelectError ))
    | NewClientConnection ClientId QueryExpression
    | ClientQueryUpdate ClientId QueryExpression {- old query -} QueryExpression {- new query -}
    | ClientDisconnected ClientId


newState : (StateUpdate -> Cmd Msg) -> KnownValue a -> Model -> ( Model, Maybe ServerEvent, Cmd Msg )
newState stateUpdates (KnownValue _ queryResponse) model =
    update stateUpdates (StateUpdate queryResponse) model


update : (StateUpdate -> Cmd Msg) -> Msg -> Model -> ( Model, Maybe ServerEvent, Cmd Msg )
update stateUpdates msg model =
    case msg of
        ClientMessage { clientId, message } ->
            let
                parseResult =
                    Query.parseQuery message

                oldQuery =
                    IntDict.get clientId model.clientQueries

                serverEvent =
                    case parseResult of
                        Ok newQuery ->
                            Maybe.map (\oldQuery_ -> ClientQueryUpdate clientId oldQuery_ newQuery) oldQuery
                                |> Maybe.withDefault (NewClientConnection clientId newQuery)

                        Err msg ->
                            QueryParseFail clientId msg
            in
                ( { model
                    | clientQueries =
                        parseResult
                            |> Result.map (\query -> IntDict.insert clientId query model.clientQueries)
                            |> Result.withDefault model.clientQueries
                  }
                , Just serverEvent
                , Cmd.none
                )

        ClientDisconnect clientId ->
            ( { model | clientQueries = IntDict.remove clientId model.clientQueries }
            , Just (ClientDisconnected clientId)
            , Cmd.none
            )

        StateUpdate newState ->
            let
                successfulSelectResults : Int -> SelectResult -> StateUpdateEvent -> StateUpdateEvent
                successfulSelectResults clientId result state =
                    case result of
                        Ok response ->
                            let
                                rawResponse =
                                    Query.queryResponseToString response
                            in
                                List.foldl
                                    (\clientMessage ( stateUpdate, hasAdded ) ->
                                        if hasAdded || clientMessage.message /= rawResponse then
                                            ( clientMessage :: stateUpdate, hasAdded )
                                        else
                                            ( { clientMessage
                                                | clients = clientId :: clientMessage.clients
                                              }
                                                :: stateUpdate
                                            , True
                                            )
                                    )
                                    ( [], False )
                                    state.stateUpdate
                                    |> (\( stateUpdate, added ) ->
                                            if added then
                                                { stateUpdate = stateUpdate
                                                , errors = state.errors
                                                }
                                            else
                                                { stateUpdate = { clients = [ clientId ], message = rawResponse } :: stateUpdate
                                                , errors = state.errors
                                                }
                                       )

                        Err msg ->
                            { state | errors = ( clientId, msg ) :: state.errors }

                clientUpdates =
                    IntDict.map (\_ query -> Query.select query newState) model.clientQueries
                        |> IntDict.foldl successfulSelectResults { stateUpdate = [], errors = [] }

                cmd =
                    stateUpdates clientUpdates.stateUpdate
            in
                if List.isEmpty clientUpdates.errors then
                    ( model, Nothing, cmd )
                else
                    ( model, Just (SelectFail clientUpdates.errors), cmd )

        _ ->
            ( model, Nothing, Cmd.none )



-- Subscriptions


subscriptions : ((Json.Value -> Msg) -> Sub Msg) -> ((Int -> Msg) -> Sub Msg) -> Model -> Sub Msg
subscriptions clientMessages clientDisconnects model =
    Sub.batch
        [ clientMessages (Json.decodeValue clientMsgDecoder >> Result.map ClientMessage >> Result.withDefault NoOp)
        , clientDisconnects ClientDisconnect
        ]
