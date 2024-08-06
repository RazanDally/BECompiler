(*This benchmark tests the algorithm's ability to handle type aliases and detect a type error when a value of an incompatible type is used where a type alias is expected.*)

(*////////////////// Did not show error //////////////////*)

type int_pair = int * int ;;

let swap (x, y) = (y, x) ;;

let main () = swap (1, true) ;;
