module Game
    exposing
        ( Game
        , board
        , claimCellAt
        , finalWinner
        , isInSession
        , new
        , player
        )

import Board exposing (Board)
import Coordinates exposing (Coordinates)
import Player exposing (Player(..))


type Game
    = Game
        { player : Player
        , board : Board
        , winner : Player
        }


new : Int -> Game
new count =
    Game
        { player = X
        , board = Board.new count
        , winner = Nobody
        }


claimCellAt : Player -> Game -> Coordinates -> Game
claimCellAt claimant game coordinates =
    if validateMove coordinates claimant game then
        board game
            |> Board.setCellOwnerAt coordinates (player game)
            |> flip updateBoard game
            |> setNextPlayer
            |> setWinner
    else
        game


validateMove : Coordinates -> Player -> Game -> Bool
validateMove coordinates claimant game =
    Board.cellAt (board game) coordinates
        |> Maybe.map (Board.isCellOpen >> (&&) (claimant == player game) >> (&&) (isInSession game))
        |> Maybe.withDefault False


isInSession : Game -> Bool
isInSession game =
    winner game
        == Nobody
        && (board game |> Board.openCells |> List.length |> (<) 0)


setWinner : Game -> Game
setWinner game =
    board game |> Board.findWinner |> flip updateWinner game


board : Game -> Board
board (Game data) =
    data.board


finalWinner : Game -> Maybe Player
finalWinner game =
    case ( isInSession game, winner game ) of
        ( True, _ ) ->
            Nothing

        ( False, winner ) ->
            Just winner


player : Game -> Player
player (Game data) =
    data.player


winner : Game -> Player
winner (Game data) =
    data.winner


updateBoard : Board -> Game -> Game
updateBoard board (Game data) =
    Game { data | board = board }


updateWinner : Player -> Game -> Game
updateWinner winner (Game data) =
    Game { data | winner = winner }


updatePlayer : Player -> Game -> Game
updatePlayer player (Game data) =
    Game { data | player = player }


setNextPlayer : Game -> Game
setNextPlayer game =
    player game |> Player.next |> flip updatePlayer game
