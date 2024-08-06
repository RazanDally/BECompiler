(*This benchmark tests the algorithm's ability to handle mutually recursive functions and detect a type error when an incompatible argument type is passed to one of the functions*)
let rec even n = if n = 0 then true else odd (n - 1) 
and odd n = if n = 0 then false else even (n - 1) ;;

let main () = even true ;;
