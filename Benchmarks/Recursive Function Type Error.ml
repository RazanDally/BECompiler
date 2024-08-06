(*This benchmark tests the algorithm's ability to handle recursive functions
 and detect a type error when an incompatible argument type is passed to a recursive function.*)
let rec factorial n =
  if n = 0 then 1 else n * factorial (n - 1) ;;
let main () = factorial true ;;
