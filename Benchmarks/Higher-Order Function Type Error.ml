(*This benchmark tests the algorithm's ability to handle higher-order functions and detect a type error
 when an incompatible argument type is passed to a function that expects a function as its argument.*)

 let twice f x = f (f x) ;;
let main () = twice (fun x -> x + 1) true ;;
