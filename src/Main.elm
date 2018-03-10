module Main exposing (main)

import Board exposing (Board, Cell)
import ComputerPlayer
import Game exposing (Game, board)
import Html exposing (Html, button, div, h1, img, span, table, td, text, tr)
import Html.Attributes exposing (class, classList)
import Html.Events exposing (onClick)
import Player exposing (Player(..))
import Process
import Task


---- MODEL ----


type alias Model =
    { game : Game
    , mode : Mode
    , player : Player
    }


initialModel : Model
initialModel =
    { game = initialGame
    , mode = TwoPlayer
    , player = X
    }


initialGame : Game
initialGame =
    Game.new 3


init : ( Model, Cmd Msg )
init =
    ( initialModel
    , Cmd.none
    )


type Mode
    = TwoPlayer
    | OnePlayer


nextPlayer : Model -> Player
nextPlayer model =
    case model.mode of
        OnePlayer ->
            model.player

        TwoPlayer ->
            Player.next model.player


nextMoveCmd : Model -> Cmd Msg
nextMoveCmd model =
    case model.mode of
        OnePlayer ->
            Task.perform (always GetComputerMove) (Process.sleep 600)

        TwoPlayer ->
            Cmd.none



---- UPDATE ----


type Msg
    = InitiateGame Mode
    | ClaimCell Cell
    | GetComputerMove


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        InitiateGame mode ->
            ( { model
                | game = initialGame
                , mode = mode
                , player = X
              }
            , Cmd.none
            )

        ClaimCell cell ->
            ( { model
                | game =
                    Board.cellCoordinates cell |> Game.claimCellAt model.player model.game
                , player = nextPlayer model
              }
            , nextMoveCmd model
            )

        GetComputerMove ->
            ( { model
                | game =
                    model.game
                        |> ComputerPlayer.getNextMove
                        |> Game.claimCellAt O model.game
              }
            , Cmd.none
            )



---- VIEW ----


view : Model -> Html Msg
view model =
    div [ class "game-panel" ]
        [ restartButtons model
        , h1 [] [ text "tic tac toe" ]
        , gameView model.game
        , turnView model.game
        ]


restartButtons : Model -> Html Msg
restartButtons model =
    div [ class "restart-panel" ]
        (List.map
            (newGameButton model)
            [ TwoPlayer, OnePlayer ]
        )


newGameButton : Model -> Mode -> Html Msg
newGameButton model mode =
    button
        [ onClick <| InitiateGame mode
        , classList [ ( "highlighted", mode == model.mode ) ]
        ]
        [ text <| modeDisplay mode ]


modeDisplay : Mode -> String
modeDisplay mode =
    case mode of
        OnePlayer ->
            "1 PLAYER"

        TwoPlayer ->
            "2 PLAYERS"


gameView : Game -> Html Msg
gameView game =
    div []
        [ boardView (Game.board game)
        ]


turnView : Game -> Html Msg
turnView game =
    div [ class "turn-panel" ]
        (case ( Game.finalWinner game, Game.player game ) of
            ( Just Nobody, _ ) ->
                [ span [ class "highlighted" ]
                    [ text "It's a draw!" ]
                ]

            ( Just winner, _ ) ->
                [ playerNameDisplay winner
                , span [] [ text " wins!" ]
                ]

            ( Nothing, player ) ->
                [ span [] [ text "turn: " ]
                , playerNameDisplay player
                ]
        )


playerNameDisplay : Player -> Html Msg
playerNameDisplay player =
    let
        str =
            case player of
                X ->
                    "X"

                O ->
                    "O"

                _ ->
                    "nobody's"
    in
    span [ class "highlighted" ] [ text str ]


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
            text ""

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
