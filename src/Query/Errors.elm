module Query.Errors exposing (errorMessageEnglish)

{-|
@docs errorMessageEnglish
-}

import Internal.Types exposing (ResponseParseError(..))


{-| Get an error message in English explaining what went wrong when trying to parse
a query response.
-}
errorMessageEnglish : ResponseParseError -> String
errorMessageEnglish error =
    -- TODO: don't use raw values
    case error of
        WrongType expected actual ->
            "Type mismatch -- wanted: " ++ toString expected ++ ", got: " ++ toString actual

        ExpectedKnown expected actual ->
            "Expected a value: " ++ toString expected ++ ", got: " ++ toString actual

        BaseCaseForOneOf ->
            "No query options (empty list) supplied to Query.oneOf"

        ErrorAtIndex index innerError ->
            "Error parsing index " ++ toString index ++ " of a list:\n" ++ errorMessageEnglish innerError



-- TODO: error messages in other languages ???
