module Main exposing (..)

import Html


(~=) s1 s2 =
    let
        firstLetter =
            String.left 1
    in
        firstLetter s1 == firstLetter s2


main =
    "Gee B"
        ~= "Gero"
        |> toString
        |> Html.text
