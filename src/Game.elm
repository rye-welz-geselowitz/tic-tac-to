module Game exposing (Game, board, claimCell, new, player, winner)

import Board exposing (Board, Cell)
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


claimCell : Cell -> Player -> Game -> Game
claimCell cell claimant game =
    if Board.isCellOpen cell && claimant == player game && isInSession game then
        board game
            |> Board.setCellOwner cell (player game)
            |> flip updateBoard game
            |> setNextPlayer
            |> setWinner
    else
        game


isInSession : Game -> Bool
isInSession =
    winner >> (==) Nobody


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
    player game |> Player.next |> flip updatePlayer game
