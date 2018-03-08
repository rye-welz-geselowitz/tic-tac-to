module Player exposing (Player(..), getNext)


type Player
    = X
    | O
    | Nobody


getNext : Player -> Player
getNext player =
    case player of
        X ->
            O

        O ->
            X

        Nobody ->
            X
