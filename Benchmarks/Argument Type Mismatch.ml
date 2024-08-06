(*This benchmark tests the algorithm's ability to detect and report a level-0 error,
 where a value of an incompatible type (bool) is passed as an argument to a function expecting a different type (int).*)
let add x y = x + y ;;
let main () = add true 3 ;; 
