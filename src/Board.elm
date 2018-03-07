module Board exposing (Board, new, Cell, Row, rows, cellOwner, claimCell)

import Player exposing (Player(..))

type Board =
  Board (List Row)

type alias Row = (List Cell)

type Cell =
  Cell Coordinates Player

type alias Coordinates = (RowIndex, ColumnIndex)

type alias RowIndex = Int
type alias ColumnIndex = Int

cellOwner : Cell ->  Player
cellOwner (Cell _ owner) =
  owner

updateCellOWner :  Player -> Cell -> Cell
updateCellOWner owner (Cell coordinates _) =
  Cell coordinates owner

cellCoordinates : Cell -> Coordinates
cellCoordinates (Cell coordinates _) =
  coordinates

cellRowIdx : Cell -> RowIndex
cellRowIdx =
  cellCoordinates >> Tuple.first


cellColIdx : Cell -> RowIndex
cellColIdx =
  cellCoordinates >> Tuple.second

rows : Board -> List Row
rows (Board rows) =
  rows

updateRows : List Row -> Board -> Board
updateRows rows (Board _) =
  Board rows

new : Int -> Int -> Board
new rowCount colCount =
  List.range 0 (rowCount - 1)
    |> List.map (\rowIdx ->
      List.range 0 (colCount - 1)
        |> List.map (\colIdx ->
          Cell (rowIdx, colIdx) None
        )
      )
    |> Board

updateAt : Int -> (a -> a) -> List a -> List a
updateAt  targetIdx fnUpdate list=
  list |> List.indexedMap (\idx item ->
    if (targetIdx == idx) then
      fnUpdate item
    else
      item
    )

claimCell: Cell -> Player -> Board -> Board
claimCell cell player board =
  let
   newRows =
     rows board
     |> updateAt (cellRowIdx cell) (updateAt (cellColIdx cell) (updateCellOWner player))
  in
  updateRows newRows board
