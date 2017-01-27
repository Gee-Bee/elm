module Main exposing (..)

import Html


cart : List { name : String, qty : Int, freeQty : Int }
cart =
    [ { name = "Lemon", qty = 1, freeQty = 0 }
    , { name = "Apple", qty = 5, freeQty = 0 }
    , { name = "Pear", qty = 10, freeQty = 0 }
    ]


setFreeQty : { name : String, qty : Int, freeQty : Int } -> { name : String, qty : Int, freeQty : Int }
setFreeQty item =
    if item.qty >= 10 then
        { item | freeQty = 3 }
    else if item.qty >= 5 then
        { item | freeQty = 1 }
    else
        item


main =
    List.map setFreeQty cart
        |> toString
        |> Html.text
