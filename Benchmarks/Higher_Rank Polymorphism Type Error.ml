(*This benchmark tests the algorithm's ability to handle higher-rank polymorphism and detect a type error when a function that expects a higher-rank polymorphic type is applied to an argument of an incompatible type*)

let apply_twice f x = f (f x) ;;

let main () =
  let add_one x = x + 1 in
  apply_twice add_one true ;;
