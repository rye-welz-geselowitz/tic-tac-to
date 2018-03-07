module Player exposing (Player(..), getNext)

type Player =
  X | O | None

getNext : Player -> Player
getNext player =
  case player of
    X -> O
    O -> X
    None -> X --TODO: think about this
