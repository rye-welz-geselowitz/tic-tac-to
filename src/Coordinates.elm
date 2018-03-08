module Coordinates
    exposing
        ( Coordinates
        , Direction(..)
        , columnIndex
        , getCellNeighborCoordinates
        , oppositeDirection
        , rowIndex
        )


type alias Coordinates =
    ( Int, Int )


rowIndex : Coordinates -> Int
rowIndex =
    Tuple.first


columnIndex : Coordinates -> Int
columnIndex =
    Tuple.second


getCellNeighborCoordinates : Direction -> Coordinates -> Coordinates
getCellNeighborCoordinates direction =
    case direction of
        --TODO: consolodate?!?! EXTRACT coordinates!!! RENAME!!!
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
