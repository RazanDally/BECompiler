(*This benchmark tests the algorithm's ability to handle type classes and detect a type error when an incompatible type is used with a type class method.*)
class virtual ['a] printable =
  object
    method virtual print : 'a -> unit
  end ;;

class int_printer =
  object
    inherit [int] printable
    method print x = print_int x
  end ;;

class string_printer =
  object
    inherit [string] printable
    method print x = print_endline x
  end ;;

let main () =
  let printer = int_printer in printer#print "hello" ;;
