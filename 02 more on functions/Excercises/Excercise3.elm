module Main exposing (..)

import Html


wordCount =
    String.words >> List.length


main =
    wordCount "ala ma kota"
        |> toString
        |> Html.text
