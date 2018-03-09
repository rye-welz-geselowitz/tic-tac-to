module Game exposing (Game, board, claimCellAt, new, player, winner)

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
