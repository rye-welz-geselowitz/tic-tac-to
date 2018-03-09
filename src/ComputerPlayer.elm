module ComputerPlayer exposing (getNextMove)

import Board
import Coordinates exposing (Coordinates)
import Game exposing (Game)


getNextMove : Game -> Coordinates
getNextMove =
    Game.board >> Board.openCells >> List.head >> Maybe.map Board.cellCoordinates >> Maybe.withDefault ( 0, 0 )
