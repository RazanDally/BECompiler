(*This benchmark tests the algorithm's ability to handle polymorphic functions and detect a type error when a polymorphic function is applied to arguments of incompatible types.*)
let id x = x ;;
let main () = id true 42 ;;
