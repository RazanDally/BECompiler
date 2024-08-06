(*This benchmark tests the algorithm's ability to handle nested pattern matching and detect a type error when an incompatible type is used in the construction of a data structure pattern*)
type 'a binary_tree = Leaf of 'a | Node of 'a binary_tree * 'a binary_tree ;;

let rec sum tree =
  match tree with
  | Leaf n -> n
  | Node (left, right) ->
    match left, right with
    | Leaf l, Leaf r -> l + r
    | Node _, Node _ -> sum left + sum right
    | _, _ -> 0

let main () = sum (Node (Leaf true, Leaf 42)) ;;
