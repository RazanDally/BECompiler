(*This benchmark tests the algorithm's ability to detect and report a level-1 error, where the two branches of a conditional expression have incompatible types (int and string).*)
let test x y = if x then y else "hello" ;;
let main () = test true 42 ;;
