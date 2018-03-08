module Board
    exposing
        ( Board
        , Cell
        , cellOwner
        , cells
        , findWinner
        , isCellOpen
        , new
        , rows
        , setCellOwner
        )

import Coordinates exposing (Coordinates, Direction(..))
import Player exposing (Player(..))


{- Cell -}


type Cell
    = Cell Coordinates Player


newCellAt : Coordinates -> Cell
newCellAt coordinates =
    Cell coordinates Nobody


cellOwner : Cell -> Player
cellOwner (Cell _ owner) =
    owner


isCellOpen : Cell -> Bool
isCellOpen cell =
    cellOwner cell == Nobody


cellCoordinates : Cell -> Coordinates
cellCoordinates (Cell coordinates _) =
    coordinates


updateCellOwner : Player -> Cell -> Cell
updateCellOwner owner (Cell coordinates _) =
    Cell coordinates owner


isInOuterCorner : Cell -> Bool
isInOuterCorner cell =
    let
        ( rowIdx, colIdx ) =
            cellCoordinates cell
    in
    rowIdx == 0 || colIdx == 0


newCellFromFlatIdx : Int -> Int -> Cell
newCellFromFlatIdx n idx =
    newCellAt ( idx // n, idx % n )



{- Board -}


type Board
    = Board (List Cell)


new : Int -> Board
new n =
    List.range 0 (n * n - 1) |> List.map (newCellFromFlatIdx n) |> Board


getOuterCorner : Board -> List Cell
getOuterCorner board =
    cells board |> List.filter isInOuterCorner


rows : Board -> List (List Cell)
rows board =
    List.range 0 (size board - 1)
        |> List.map (row board)


row : Board -> Int -> List Cell
row board rowIdx =
    cells board |> List.filter (cellCoordinates >> Coordinates.rowIndex >> (==) rowIdx)


cells : Board -> List Cell
cells (Board cells) =
    cells


size : Board -> Int
size =
    cells >> List.length >> toFloat >> sqrt >> truncate


updateCells : List Cell -> Board -> Board
updateCells cells (Board _) =
    Board cells


getCellAt : Board -> Coordinates -> Maybe Cell
getCellAt board coordinates =
    cells board |> List.filter (isTargetCell coordinates) |> List.head


isTargetCell : Coordinates -> Cell -> Bool
isTargetCell coordinates =
    cellCoordinates >> (==) coordinates


updateCell : Cell -> (Cell -> Cell) -> Board -> Board
updateCell targetCell fn board =
    cells board
        |> List.map (updateIfTarget (isTargetCell <| cellCoordinates targetCell) fn)
        |> flip updateCells board


updateIfTarget : (a -> Bool) -> (a -> a) -> a -> a
updateIfTarget isTarget update item =
    if isTarget item then
        update item
    else
        item


setCellOwner : Cell -> Player -> Board -> Board
setCellOwner cell player =
    updateCell cell (updateCellOwner player)


getCellNeighbor : Cell -> Board -> Direction -> Maybe Cell
getCellNeighbor cell board direction =
    cellCoordinates cell
        |> Coordinates.getCellNeighborCoordinates direction
        |> getCellAt board


findWinner : Board -> Player
findWinner board =
    getOuterCorner board |> List.foldl (findWinnerFromCell board) Nobody


findWinnerFromCell : Board -> Cell -> Player -> Player
findWinnerFromCell board cell alreadyFoundWinner =
    case alreadyFoundWinner of
        Nobody ->
            findPathsFromCell cell board |> List.foldl (findWinnerFromPath board) Nobody

        winner ->
            winner


findWinnerFromPath : Board -> List Cell -> Player -> Player
findWinnerFromPath board path alreadyKnownWinner =
    case alreadyKnownWinner of
        Nobody ->
            checkPathWinner board path

        winner ->
            winner


checkPathWinner : Board -> List Cell -> Player
checkPathWinner board path =
    if countCellsOwnedByPlayer path X == size board then
        X
    else if countCellsOwnedByPlayer path O == size board then
        O
    else
        Nobody


countCellsOwnedByPlayer : List Cell -> Player -> Int
countCellsOwnedByPlayer cells player =
    cells
        |> List.filter (cellOwner >> (==) player)
        |> List.length


findPathsFromCell : Cell -> Board -> List (List Cell)
findPathsFromCell cell board =
    [ Above, Left, AboveLeft, AboveRight ]
        |> List.map (searchOutwards cell board)



--TODO: extract all this direction stuff!!


searchOutwards : Cell -> Board -> Direction -> List Cell
searchOutwards cell board direction =
    let
        oppositeDirection =
            Coordinates.oppositeDirection direction

        search =
            searchDirection cell board
    in
    search direction ++ (search oppositeDirection |> List.drop 1)


searchDirection : Cell -> Board -> Direction -> List Cell
searchDirection cell board direction =
    case getCellNeighbor cell board direction of
        Nothing ->
            [ cell ]

        Just neighbor ->
            cell :: searchDirection neighbor board direction
