module Coordinates
    exposing
        ( Coordinates
        , Direction
        , left
        , searchOutwards
        , up
        , upLeft
        , upRight
        )

{- Types -}


type alias Coordinates =
    ( Int, Int )


type Direction
    = Direction (List BasicDirection)


type BasicDirection
    = Up
    | Down
    | Left
    | Right



{- Default directions -}


up : Direction
up =
    Direction [ Up ]


down : Direction
down =
    Direction [ Down ]


left : Direction
left =
    Direction [ Left ]


right : Direction
right =
    Direction [ Right ]


upRight : Direction
upRight =
    Direction [ Up, Right ]


upLeft : Direction
upLeft =
    Direction [ Up, Left ]



{- Search coordinate system -}


searchOutwards :
    item
    -> (item -> Coordinates)
    -> (Coordinates -> Maybe item)
    -> Direction
    -> List item
searchOutwards item getCoordinates itemAt direction =
    let
        search =
            searchDirection item getCoordinates itemAt
    in
    search direction ++ (oppositeDirection direction |> search |> List.drop 1)



-- HELPERS


oppositeDirection : Direction -> Direction
oppositeDirection (Direction directions) =
    List.map oppositeBasicDirection directions |> Direction


oppositeBasicDirection : BasicDirection -> BasicDirection
oppositeBasicDirection direction =
    case direction of
        Up ->
            Down

        Down ->
            Up

        Left ->
            Right

        Right ->
            Left


basicDirectionMove : BasicDirection -> Coordinates -> Coordinates
basicDirectionMove direction =
    case direction of
        Up ->
            getCoordinatesAbove

        Down ->
            getCoordinatesBelow

        Left ->
            getCoordinatesLeft

        Right ->
            getCoordinatesRight


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


searchDirection :
    item
    -> (item -> Coordinates)
    -> (Coordinates -> Maybe item)
    -> Direction
    -> List item
searchDirection item getCoordinates itemAt direction =
    case neighbor getCoordinates direction itemAt item of
        Nothing ->
            [ item ]

        Just neighbor ->
            item :: searchDirection neighbor getCoordinates itemAt direction


neighbor :
    (item -> Coordinates)
    -> Direction
    -> (Coordinates -> Maybe item)
    -> item
    -> Maybe item
neighbor getCoordinates direction itemAt =
    getCoordinates >> neighborCoordinates direction >> itemAt


neighborCoordinates : Direction -> Coordinates -> Coordinates
neighborCoordinates (Direction directions) =
    List.foldl (\direction fn -> fn >> basicDirectionMove direction) identity directions
