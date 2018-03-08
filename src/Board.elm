module Board
    exposing
        ( Board
        , Cell
        , cellOwner
        , findWinner
        , isCellOpen
        , new
        , openCells
        , rows
        , setCellOwner
        )

import Coordinates exposing (Coordinates, Direction)
import Player exposing (Player(..))


{- Types -}


type Board
    = Board (List Cell)


type Cell
    = Cell Coordinates Player



-- CELL
{- Exposed -}


cellOwner : Cell -> Player
cellOwner (Cell _ owner) =
    owner


isCellOpen : Cell -> Bool
isCellOpen cell =
    cellOwner cell == Nobody


setCellOwner : Cell -> Player -> Board -> Board
setCellOwner cell player =
    updateCell cell (updateCellOwner player)



-- BOARD
{- Exposed -}


new : Int -> Board
new n =
    List.range 0 (n * n - 1) |> List.map (newCellFromFlatIdx n) |> Board


rows : Board -> List (List Cell)
rows board =
    List.range 0 (size board - 1)
        |> List.map (row board)


findWinner : Board -> Player
findWinner board =
    getOuterCorner board |> List.foldl (findWinnerFromCell board) Nobody


openCells : Board -> List Cell
openCells board =
    cells board |> List.filter isCellOpen



-- HELPERS


newCellAt : Coordinates -> Cell
newCellAt coordinates =
    Cell coordinates Nobody


newCellFromFlatIdx : Int -> Int -> Cell
newCellFromFlatIdx n idx =
    newCellAt ( idx // n, idx % n )


cellCoordinates : Cell -> Coordinates
cellCoordinates (Cell coordinates _) =
    coordinates


updateCellOwner : Player -> Cell -> Cell
updateCellOwner owner (Cell coordinates _) =
    Cell coordinates owner


getOuterCorner : Board -> List Cell
getOuterCorner board =
    cells board |> List.filter isInOuterCorner


isInOuterCorner : Cell -> Bool
isInOuterCorner cell =
    let
        ( rowIdx, colIdx ) =
            cellCoordinates cell
    in
    rowIdx == 0 || colIdx == 0


cells : Board -> List Cell
cells (Board cells) =
    cells


row : Board -> Int -> List Cell
row board rowIdx =
    cells board |> List.filter (cellCoordinates >> Tuple.first >> (==) rowIdx)


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
    [ Coordinates.up, Coordinates.left, Coordinates.upLeft, Coordinates.upRight ]
        |> List.map (searchOutwards cell board)


searchOutwards : Cell -> Board -> Direction -> List Cell
searchOutwards cell board =
    Coordinates.searchOutwards cell cellCoordinates (getCellAt board)


updateIfTarget : (a -> Bool) -> (a -> a) -> a -> a
updateIfTarget isTarget update item =
    if isTarget item then
        update item
    else
        item
