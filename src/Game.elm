module Game exposing (Game, board, claimCell, new, player, winner)

import Board exposing (Board, Cell)
import Player exposing (Player(..))


type Game
    = Game
        { player : Player
        , board : Board
        , winner : Player
        , isActive : Bool --TODO: give this meaning!
        }


new : Int -> Game
new count =
    Game
        { player = X
        , board = Board.new count
        , winner = Nobody
        , isActive = False
        }


claimCell : Cell -> Game -> Game
claimCell cell game =
    if Board.isCellOpen cell then
        board game
            |> Board.setCellOwner cell (player game)
            |> flip updateBoard game
            |> setNextPlayer
            |> setWinner
    else
        game


setWinner : Game -> Game
setWinner game =
    updateWinner (Board.findWinner (board game)) game


board : Game -> Board
board (Game data) =
    data.board


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
    player game |> Player.getNext |> flip updatePlayer game
