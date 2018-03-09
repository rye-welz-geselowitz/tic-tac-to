module ComputerPlayer exposing (getNextMove)

import Board exposing (Cell)
import Game exposing (Game)


getNextMove : Game -> Cell
getNextMove =
    Game.board >> Board.openCells >> List.head >> Maybe.withDefault Board.topCornerCell
