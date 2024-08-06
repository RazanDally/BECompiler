(*This benchmark tests the algorithm's ability to handle functors and detect a type error when a functor is applied to a module with an incompatible type*)

module type PRINTABLE = sig
  type t
  val print : t -> unit
end ;;

module PrintableInt = struct
  type t = int
  let print x = print_int x
end ;;

module IntPrinter (X : PRINTABLE) = struct
  let print_twice x = X.print x; X.print x
end ;;

let main () =
  let module P = IntPrinter(PrintableInt) in
  P.print_twice true ;;
