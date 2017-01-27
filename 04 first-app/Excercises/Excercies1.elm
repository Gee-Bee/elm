module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- model


type alias Model =
    { calories : Int
    , caloriesHistory : List Int
    , input : Int
    , error : Maybe String
    }


initModel : Model
initModel =
    Model 0 [] 0 Nothing



-- update


type Msg
    = Input String
    | AddCalorie
    | Clear


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input val ->
            case String.toInt val of
                Ok input ->
                    { model
                        | input = input
                        , error = Nothing
                    }

                Err err ->
                    { model
                        | input = initModel.input
                        , error = Just err
                    }

        AddCalorie ->
            { model
                | calories = model.calories + model.input
                , input = initModel.input
                , caloriesHistory = model.caloriesHistory ++ [ model.input ]
            }

        Clear ->
            initModel



-- view


view : Model -> Html Msg
view model =
    div []
        [ h3 []
            [ text ("Total Calories: " ++ (toString model.calories)) ]
        , input
            [ type_ "text"
            , onInput Input
            , value
                (if model.input == 0 then
                    ""
                 else
                    toString model.input
                )
            ]
            []
        , div []
            [ text (Maybe.withDefault "" model.error) ]
        , button
            [ type_ "button"
            , onClick AddCalorie
            ]
            [ text "Add" ]
        , button
            [ type_ "button"
            , onClick Clear
            ]
            [ text "Clear" ]
        , ul []
            (List.map
                (\calories -> li [] [ text ("You ate " ++ (toString calories) ++ " calories") ])
                model.caloriesHistory
            )
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , update = update
        , view = view
        }
