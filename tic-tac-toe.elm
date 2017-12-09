module TicTacToe exposing (Model, Msg, init, update, view)

import Array exposing (..)
import Html exposing (..)
import Html.Events exposing (onClick)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }


type Player
    = O
    | X


type alias Grid =
    Array (Maybe Player)


type alias Model =
    { grid : Grid
    , currentTurn : Player
    , winner : Maybe Player
    }


type Msg
    = TakeTurn Int
    | Reset


init : ( Model, Cmd Msg )
init =
    ( resetModel, Cmd.none )


resetModel : Model
resetModel =
    Model
        (Array.repeat 9 Nothing)
        O
        Nothing


nextPlayer : Player -> Player
nextPlayer current =
    case current of
        O ->
            X

        X ->
            O


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TakeTurn n ->
            let
                newGrid =
                    set n (Just model.currentTurn) model.grid
            in
            ( { model
                | grid = newGrid
                , winner = checkWinner newGrid
                , currentTurn = nextPlayer model.currentTurn
              }
            , Cmd.none
            )

        Reset ->
            ( resetModel, Cmd.none )


checkWinner : Grid -> Maybe Player
checkWinner grid =
    List.filterMap identity
        [ -- rows
          checkWin grid 0 1 2
        , checkWin grid 3 4 5
        , checkWin grid 6 7 8

        -- columns
        , checkWin grid 0 3 6
        , checkWin grid 1 4 7
        , checkWin grid 2 5 8

        -- diagonals
        , checkWin grid 0 4 8
        , checkWin grid 2 4 6
        ]
        |> List.head


checkWin : Grid -> Int -> Int -> Int -> Maybe Player
checkWin grid a b c =
    let
        aCell =
            get a grid

        bCell =
            get b grid

        cCell =
            get c grid
    in
    if aCell == bCell && aCell == cCell then
        Maybe.withDefault Nothing aCell
    else
        Nothing


checkDraw : Grid -> Bool
checkDraw grid =
    Array.toList grid
        |> List.all (\x -> x /= Nothing)


view : Model -> Html Msg
view model =
    div []
        [ pre []
            [ viewNaughtOrCross model 0
            , text "|"
            , viewNaughtOrCross model 1
            , text "|"
            , viewNaughtOrCross model 2
            , br [] []
            , viewNaughtOrCross model 3
            , text "|"
            , viewNaughtOrCross model 4
            , text "|"
            , viewNaughtOrCross model 5
            , br [] []
            , viewNaughtOrCross model 6
            , text "|"
            , viewNaughtOrCross model 7
            , text "|"
            , viewNaughtOrCross model 8
            ]
        , h1 [] [ text (viewStatus model) ]
        , button [ onClick Reset ] [ text "Reset" ]
        ]


viewStatus : Model -> String
viewStatus model =
    case model.winner of
        Just player ->
            toString player ++ " has won!"

        Nothing ->
            if checkDraw model.grid then
                "It's a draw!"
            else
                toString model.currentTurn ++ "'s turn"


viewNaughtOrCross : Model -> Int -> Html Msg
viewNaughtOrCross model n =
    let
        cell =
            Maybe.withDefault Nothing (get n model.grid)
    in
    case cell of
        Just player ->
            span [] [ text (toString player) ]

        Nothing ->
            span
                (if model.winner == Nothing then
                    [ onClick (TakeTurn n) ]
                 else
                    []
                )
                [ text "_" ]
