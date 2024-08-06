(*This benchmark tests the algorithm's ability to handle nested data structures (in this case, a binary tree) and detect a type error when an incompatible type is used in the construction of the data structure.*)
type 'a tree = Leaf of 'a | Node of 'a tree * 'a * 'a tree ;;

let depth tree =
  match tree with
  | Leaf _ -> 1
  | Node (left, _, right) -> 1 + max (depth left) (depth right) ;;

let main () = depth (Node (Leaf 1, true, Leaf 2)) ;;
