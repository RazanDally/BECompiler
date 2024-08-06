(*This benchmark tests the algorithm's ability to handle Generalized Algebraic Data Types (GADTs) and detect a type error when an incompatible type is used in the construction or pattern matching of a GADT.*)

type _ term =
  | Const : int -> int term
  | Add : int term * int term -> int term
  | Bool : bool -> bool term ;;

let eval t =
  match t with
  | Const n -> n
  | Add (t1, t2) -> eval t1 + eval t2
  | Bool b -> if b then 1 else 0 ;;

let main () = eval (Add (Const 1, Bool true)) ;;
