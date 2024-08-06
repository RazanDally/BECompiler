(*This benchmark tests the algorithm's ability to handle variant types and detect a type error when an incompatible type is used in the construction or pattern matching of a variant type*)

type expr =
  | Const of int
  | Add of expr * expr
  | Bool of bool

let eval expr =
  match expr with
  | Const n -> n
  | Add (e1, e2) -> eval e1 + eval e2
  | Bool b -> if b then 1 else 0 ;;

let main () = eval (Add (Const 1, Bool true)) ;;
