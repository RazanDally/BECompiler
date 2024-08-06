(*This benchmark tests the algorithm’s ability to detect and report a level-2 error, where multiple conflicting data flows converge, leading to a type mismatch.*)
let apply f x = f x ;;
let test x y = apply (fun z -> z) y, apply (fun z -> z + 1) x ;;
let main () = test true 42 ;;

