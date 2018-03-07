module Board exposing (Board, Cell, Row, cellOwner, isCellOpen, new, rows, updateCellOwner)

import Player exposing (Player(..))


type Board
    = Board (List Row)


type alias Row =
    List Cell


type Cell
    = Cell Coordinates Player


type alias Coordinates =
    ( RowIndex, ColumnIndex )


type alias RowIndex =
    Int


type alias ColumnIndex =
    Int


cellOwner : Cell -> Player
cellOwner (Cell _ owner) =
    owner


isCellOpen : Cell -> Bool
isCellOpen cell =
    cellOwner cell == Nobody


updateCellOWner : Player -> Cell -> Cell
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
    generateInRange (rowCount - 1)
        (\rowIdx -> generateInRange (colCount - 1) (\colIdx -> Cell ( rowIdx, colIdx ) Nobody))
        |> Board


generateInRange : Int -> (Int -> a) -> List a
generateInRange upperBound fn =
    --TODO: better name
    List.range 0 upperBound |> List.map fn


updateAt : Int -> (a -> a) -> List a -> List a
updateAt targetIdx fnUpdate list =
    list
        |> List.indexedMap
            (\idx item ->
                if targetIdx == idx then
                    fnUpdate item
                else
                    item
            )


getCellAt : Board -> Coordinates -> Maybe Cell
getCellAt board coordinates =
    --TODO: clean up!
    rows board
        |> List.concatMap identity
        |> List.foldl
            (\cell acc ->
                case ( acc, cellCoordinates cell == coordinates ) of
                    ( Nothing, True ) ->
                        Just cell

                    ( acc_, _ ) ->
                        acc
            )
            Nothing


isCellInWinningPath : Cell -> Board -> Bool
isCellInWinningPath cell board =
    False


updateCell : Cell -> (Cell -> Cell) -> Board -> Board
updateCell cell fn board =
    let
        newRows =
            rows board |> updateAt (cellRowIdx cell) (updateAt (cellColIdx cell) fn)
    in
    board |> updateRows newRows


updateCellOwner : Cell -> Player -> Board -> Board
updateCellOwner cell player =
    updateCell cell (updateCellOWner player)



-- getCellNeighbors : Cell -> Board -> List Cell
-- getCellNeighbors cell board =
--     [ Above, Below, Left, Right ] |> List.filterMap (getCellNeighbor cell board)


getCellNeighbor : Cell -> Board -> Direction -> Maybe Cell
getCellNeighbor cell board direction =
    getCellNeighborCoordinates direction cell
        |> getCellAt board


getCellNeighborCoordinates : Direction -> Cell -> Coordinates
getCellNeighborCoordinates direction cell =
    let
        ( rowIdx, colIdx ) =
            cellCoordinates cell
    in
    case direction of
        --TODO: consolodate?!?!
        Above ->
            ( rowIdx - 1, colIdx )

        Below ->
            ( rowIdx + 1, colIdx )

        Left ->
            ( rowIdx, colIdx - 1 )

        Right ->
            ( rowIdx, colIdx + 1 )

        AboveLeft ->
            ( rowIdx - 1, colIdx - 1 )

        AboveRight ->
            ( rowIdx - 1, colIdx + 1 )

        BelowLeft ->
            ( rowIdx + 1, colIdx - 1 )

        BelowRight ->
            ( rowIdx + 1, colIdx + 1 )


type Direction
    = Above
    | Below
    | Left
    | Right
    | AboveLeft
    | AboveRight
    | BelowLeft
    | BelowRight
