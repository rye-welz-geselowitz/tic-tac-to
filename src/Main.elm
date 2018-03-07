module Main exposing (main)

import Html exposing (Html, text, div, img, button, table, tr, td)
import Html.Events exposing (onClick)
import Board exposing (Board, Row, Cell)
import Game exposing (Game, board)
import Player exposing (Player(..))
---- MODEL ----




init : ( Model, Cmd Msg )
init =
    ( {
      game = Game.new 3 3

    },
    Cmd.none )


type alias Model =
    {
      game: Game
    }


---- UPDATE ----


type Msg
    = ClaimCell Cell


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
      ClaimCell cell ->
        ({ model | game = Game.claimCell cell model.game}, Cmd.none)



---- VIEW ----


view : Model -> Html Msg
view model =
    div []
        [boardView (Game.board model.game)]


boardView : Board -> Html Msg
boardView board =
  table []
    (Board.rows board |> List.map rowView)

rowView : Row -> Html Msg
rowView row =
  tr []
    (row |> List.map cellView)

cellView : Cell -> Html Msg
cellView cell =
  td [onClick <| ClaimCell cell] [cellContents cell]

cellContents : Cell -> Html Msg
cellContents cell =
  case Board.cellOwner cell of
    None -> text "-"
    X -> text "x"
    O -> text "o"

---- PROGRAM ----


main : Program Never Model Msg
main =
    Html.program
        { view = view
        , init = init
        , update = update
        , subscriptions = always Sub.none
        }
