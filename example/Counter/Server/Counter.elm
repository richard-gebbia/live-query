module Counter exposing (..)

import Query.Known exposing (KnownValue)
import Time


-- Model


type alias Model =
    Int


init : Model
init =
    0


queryResponse : Model -> KnownValue Model
queryResponse model =
    Query.Known.int model



-- Msg


type Msg
    = Increment



-- Update


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            model + 1



-- Subscriptions


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every Time.second (\_ -> Increment)
