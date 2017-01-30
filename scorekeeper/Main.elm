module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


-- model


type alias Model =
    { players : List Player
    , name : String
    , playerId : Maybe Int
    , plays : List Play
    }


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
    , playerId : Int
    , name : String
    , points : Int
    }


initModel : Model
initModel =
    { players = []
    , name = ""
    , playerId = Nothing
    , plays = []
    }



-- update


type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play


update : Msg -> Model -> Model
update msg model =
    case msg of
        Input name ->
            { model | name = name }

        Save ->
            if String.isEmpty model.name then
                model
            else
                save model

        Cancel ->
            { model | name = initModel.name, playerId = Nothing }

        Edit player ->
            { model
                | name = player.name
                , playerId = Just player.id
            }

        Score player points ->
            score model player points

        DeletePlay play ->
            deletePlay model play


save : Model -> Model
save model =
    case model.playerId of
        Nothing ->
            addPlayer model

        Just id ->
            editPlayer model id


addPlayer : Model -> Model
addPlayer model =
    let
        player =
            Player (List.length model.players) model.name 0

        newPlayers =
            player :: model.players
    in
        { model
            | name = initModel.name
            , players = newPlayers
        }


editPlayer : Model -> Int -> Model
editPlayer model id =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == id then
                        { player | name = model.name }
                    else
                        player
                )
                model.players

        newPlays =
            List.map
                (\play ->
                    if play.playerId == id then
                        { play | name = model.name }
                    else
                        play
                )
                model.plays
    in
        { model
            | name = initModel.name
            , playerId = initModel.playerId
            , players = newPlayers
            , plays = newPlays
        }


score : Model -> Player -> Int -> Model
score model scorer points =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == scorer.id then
                        { player | points = player.points + points }
                    else
                        player
                )
                model.players

        play =
            Play (List.length model.plays) scorer.id scorer.name points
    in
        { model
            | players = newPlayers
            , plays = play :: model.plays
        }


deletePlay : Model -> Play -> Model
deletePlay model play =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == play.playerId then
                        { player | points = player.points - play.points }
                    else
                        player
                )
                model.players

        newPlays =
            List.filter (\p -> p.id /= play.id) model.plays
    in
        { model
            | players = newPlayers
            , plays = newPlays
        }



-- view


view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Scorekeeper" ]
        , playerSection model
        , playerForm model
        , playSection model
        ]


playerSection : Model -> Html Msg
playerSection model =
    div []
        [ playerListHeader
        , playerList model
        , pointTotal model
        ]


playerListHeader : Html Msg
playerListHeader =
    header []
        [ div [] [ text "Name" ]
        , div [] [ text "Points" ]
        ]


playerList : Model -> Html Msg
playerList model =
    model.players
        |> List.sortBy .name
        |> List.map (player model.playerId)
        |> ul []


player : Maybe Int -> Player -> Html Msg
player editPlayerId player =
    li []
        [ i
            [ class "edit"
            , onClick (Edit player)
            ]
            []
        , div [ class (editPlayerClass editPlayerId player) ]
            [ text player.name ]
        , button
            [ type_ "button"
            , onClick (Score player 2)
            ]
            [ text "2pt" ]
        , button
            [ type_ "button"
            , onClick (Score player 3)
            ]
            [ text "3pt" ]
        , div []
            [ text (toString player.points) ]
        ]


pointTotal : Model -> Html Msg
pointTotal model =
    let
        total =
            List.map .points model.plays
                |> List.sum
    in
        footer []
            [ div [] [ text "Total:" ]
            , div [] [ text (toString total) ]
            ]


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save ]
        [ input
            [ type_ "text"
            , placeholder "Add/Edit Player..."
            , onInput Input
            , value model.name
            , class (editInputClass model.playerId)
            ]
            []
        , button
            [ type_ "submit" ]
            [ text "Save" ]
        , button
            [ type_ "button", onClick Cancel ]
            [ text "Cancel" ]
        ]


editInputClass : Maybe Int -> String
editInputClass editPlayerId =
    case editPlayerId of
        Just id ->
            "edit"

        Nothing ->
            ""


editPlayerClass : Maybe Int -> Player -> String
editPlayerClass editPlayerId player =
    case editPlayerId of
        Just id ->
            if player.id == id then
                "edit"
            else
                ""

        Nothing ->
            ""


playSection : Model -> Html Msg
playSection model =
    div []
        [ playListHeader
        , playList model
        ]


playListHeader : Html Msg
playListHeader =
    header []
        [ div [] [ text "Plays" ]
        , div [] [ text "Points" ]
        ]


playList : Model -> Html Msg
playList model =
    model.plays
        |> List.map play
        |> ul []


play : Play -> Html Msg
play play =
    li []
        [ i
            [ class "remove"
            , onClick (DeletePlay play)
            ]
            []
        , div [] [ text play.name ]
        , div [] [ text (toString play.points) ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = initModel
        , view = view
        , update = update
        }
