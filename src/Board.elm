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
    getCellNeighborCoordinates direction cell |> getCellAt board


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
checkPathWinner board cells =
    if List.length cells < size board then
        Nobody
    else
        List.tail cells
            |> Maybe.withDefault []
            |> List.foldl
                (\cell acc ->
                    if cellOwner cell == acc then
                        acc
                    else
                        Nobody
                )
                (List.head cells |> Maybe.map cellOwner |> Maybe.withDefault Nobody)



-- case cells of
--     h :: rest ->
--         if List.length cells < size board then
--             Nobody
--         else
--             rest
--                 |> List.foldl
--                     (\cell acc ->
--                         if cellOwner cell == acc then
--                             acc
--                         else
--                             Nobody
--                     )
--                     (cellOwner h)
--
--     [] ->
--         Nobody


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
