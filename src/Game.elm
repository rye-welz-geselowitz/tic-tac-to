module Game exposing (Game, board, new, claimCell)

import Board exposing (Board, Cell)
import Player exposing (Player(..))

type Game =
  Game Player Board

new : Int -> Int -> Game
new rowCount colCount =
  Game X (Board.new rowCount colCount)

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

claimCell: Cell -> Game -> Game
claimCell cell game =
  game
    |> updateBoard (board game |> Board.claimCell cell (player game))
    |> setNextPlayer
