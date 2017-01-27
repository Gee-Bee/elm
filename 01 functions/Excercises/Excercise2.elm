module Main exposing (..)

import Html


conditionalToUpper s length =
    if String.length s > length then
        String.toUpper s
    else
        s


decorate s =
    s ++ " - name length: " ++ toString (String.length s)


main =
    conditionalToUpper "Gee Bee" 6
        |> decorate
        |> Html.text
