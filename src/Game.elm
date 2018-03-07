module Game exposing (Game, board, claimCell, new)

import Board exposing (Board, Cell)
import Player exposing (Player(..))


type Game
    = Game Player Board


new : Int -> Int -> Game
new rowCount colCount =
    Game X (Board.new rowCount colCount)


claimCell : Cell -> Game -> Game
claimCell cell game =
    if Board.isCellOpen cell then
        let
            newBoard =
                board game |> Board.updateCellOwner cell (player game)
        in
        game |> updateBoard newBoard |> setNextPlayer
    else
        game


board : Game -> Board
board (Game _ board) =
    board


player : Game -> Player
player (Game player _) =
    player


updateBoard : Board -> Game -> Game
updateBoard board (Game player _) =
    Game player board


updatePlayer : Player -> Game -> Game
updatePlayer player (Game _ board) =
    Game player board


setNextPlayer : Game -> Game
setNextPlayer game =
    player game |> Player.getNext |> flip updatePlayer game
