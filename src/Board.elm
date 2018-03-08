module Board
    exposing
        ( Board
        , Cell
        , Row
        , cellOwner
        , findWinner
        , isCellOpen
        , new
        , rows
        , setCellOwner
        )

import Player exposing (Player(..))
import Utils


{- Cell -}


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


cellCoordinates : Cell -> Coordinates
cellCoordinates (Cell coordinates _) =
    coordinates


cellRowIdx : Cell -> RowIndex
cellRowIdx =
    cellCoordinates >> Tuple.first


cellColIdx : Cell -> RowIndex
cellColIdx =
    cellCoordinates >> Tuple.second


updateCellOwner : Player -> Cell -> Cell
updateCellOwner owner (Cell coordinates _) =
    Cell coordinates owner



{- Board -}


type Board
    = Board (List Row)


type alias Row =
    List Cell


new : Int -> Board
new count =
    Board <| Utils.generateListInRange (count - 1) (newRow (count - 1))


newRow : Int -> RowIndex -> Row
newRow upperBound rowIdx =
    Utils.generateListInRange upperBound (\colIdx -> Cell ( rowIdx, colIdx ) Nobody)


rows : Board -> List Row
rows (Board rows) =
    rows


size : Board -> Int
size =
    rows >> List.length


updateRows : List Row -> Board -> Board
updateRows rows (Board _) =
    Board rows


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


updateCell : Cell -> (Cell -> Cell) -> Board -> Board
updateCell cell fn board =
    let
        newRows =
            rows board |> Utils.updateListAt (cellRowIdx cell) (Utils.updateListAt (cellColIdx cell) fn)
    in
    board |> updateRows newRows


setCellOwner : Cell -> Player -> Board -> Board
setCellOwner cell player =
    updateCell cell (updateCellOwner player)


getCellNeighbor : Cell -> Board -> Direction -> Maybe Cell
getCellNeighbor cell board direction =
    cellCoordinates cell
        |> getCellNeighborCoordinates direction
        |> getCellAt board


findWinner : Board -> Player
findWinner board =
    (getTopRow board ++ getLeftColumn board)
        |> Utils.uniqify
        |> List.foldl
            (\cell alreadyFoundWinner ->
                case alreadyFoundWinner of
                    Nobody ->
                        findWinnerFromCell cell board

                    winner ->
                        winner
            )
            Nobody


getTopRow : Board -> List Cell
getTopRow board =
    List.head (rows board) |> Maybe.withDefault []


getLeftColumn : Board -> List Cell
getLeftColumn board =
    rows board
        |> List.concatMap
            (\row ->
                List.head row |> Maybe.map List.singleton |> Maybe.withDefault []
            )


findWinnerFromCell : Cell -> Board -> Player
findWinnerFromCell cell board =
    findPathsFromCell cell board
        |> List.foldl
            (\path acc ->
                case acc of
                    Nobody ->
                        checkPathWinner board path

                    winner ->
                        winner
            )
            Nobody


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


searchOutwards : Cell -> Board -> Direction -> List Cell
searchOutwards cell board direction =
    searchDirection cell board direction
        ++ searchDirection cell board (oppositeDirection direction)
        |> Utils.uniqify


searchDirection : Cell -> Board -> Direction -> List Cell
searchDirection cell board direction =
    case getCellNeighbor cell board direction of
        Nothing ->
            [ cell ]

        Just neighbor ->
            cell :: searchDirection neighbor board direction


getCellNeighborCoordinates : Direction -> Coordinates -> Coordinates
getCellNeighborCoordinates direction =
    case direction of
        --TODO: consolodate?!?! EXTRACT coordinates!!!
        Above ->
            getCoordinatesAbove

        Below ->
            getCoordinatesBelow

        Left ->
            getCoordinatesLeft

        Right ->
            getCoordinatesRight

        AboveLeft ->
            getCoordinatesAbove >> getCoordinatesLeft

        AboveRight ->
            getCoordinatesAbove >> getCoordinatesRight

        BelowLeft ->
            getCoordinatesBelow >> getCoordinatesLeft

        BelowRight ->
            getCoordinatesBelow >> getCoordinatesRight


type Direction
    = Above
    | Below
    | Left
    | Right
    | AboveLeft
    | AboveRight
    | BelowLeft
    | BelowRight


oppositeDirection : Direction -> Direction
oppositeDirection direction =
    case direction of
        Above ->
            Below

        Below ->
            Above

        Left ->
            Right

        Right ->
            Left

        AboveLeft ->
            BelowRight

        AboveRight ->
            BelowLeft

        BelowLeft ->
            AboveRight

        BelowRight ->
            AboveLeft


getCoordinatesAbove : Coordinates -> Coordinates
getCoordinatesAbove =
    addToRowCoordinate -1


getCoordinatesBelow : Coordinates -> Coordinates
getCoordinatesBelow =
    addToRowCoordinate 1


getCoordinatesLeft : Coordinates -> Coordinates
getCoordinatesLeft =
    addToColumnCoordinate -1


getCoordinatesRight : Coordinates -> Coordinates
getCoordinatesRight =
    addToColumnCoordinate 1


addToRowCoordinate : Int -> Coordinates -> Coordinates
addToRowCoordinate n ( row, col ) =
    ( row + n, col )


addToColumnCoordinate : Int -> Coordinates -> Coordinates
addToColumnCoordinate n ( row, col ) =
    ( row, col + n )
