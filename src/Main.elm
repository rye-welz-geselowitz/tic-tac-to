module Main exposing (main)

import Board exposing (Board, Cell)
import Game exposing (Game, board)
import Html exposing (Html, button, div, img, table, td, text, tr)
import Html.Events exposing (onClick)
import Player exposing (Player(..))


---- MODEL ----


init : ( Model, Cmd Msg )
init =
    ( { game = Game.new 4
      }
    , Cmd.none
    )


type alias Model =
    { game : Game
    }



---- UPDATE ----


type Msg
    = ClaimCell Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClaimCell cell ->
            ( { model | game = Game.claimCell cell model.game }, Cmd.none )



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [ gameView model.game ]


gameView : Game -> Html Msg
gameView game =
    div []
        [ boardView (Game.board game)
        , turnView game
        ]


turnView : Game -> Html Msg
turnView game =
    (case ( Game.winner game, Game.player game ) of
        ( Nobody, X ) ->
            "X turn"

        ( Nobody, O ) ->
            "O turn"

        ( X, _ ) ->
            "X won!!"

        ( O, _ ) ->
            "O won!!"

        _ ->
            "Something wrong"
    )
        |> text
        |> List.singleton
        |> div []


boardView : Board -> Html Msg
boardView board =
    table []
        (Board.rows board |> List.map rowView)


rowView : List Cell -> Html Msg
rowView row =
    tr []
        (row |> List.map cellView)


cellView : Cell -> Html Msg
cellView cell =
    td [ onClick <| ClaimCell cell ] [ cellContents cell ]


cellContents : Cell -> Html Msg
cellContents cell =
    case Board.cellOwner cell of
        Nobody ->
            text "-"

        X ->
            text "x"

        O ->
            text "o"



---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
