module Player exposing (Player(..), next)


type Player
    = X
    | O
    | Nobody


next : Player -> Player
next player =
    case player of
        X ->
            O

        O ->
            X

        Nobody ->
            X
