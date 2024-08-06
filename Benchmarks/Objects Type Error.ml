(*This benchmark tests the algorithm's ability to handle objects and detect a type error when an incompatible method is called on an object.*)

class virtual ['a] stackable =
  object
    method virtual push : 'a -> unit
    method virtual pop : unit -> 'a
  end ;;

class int_stack =
  object
    inherit ['a] stackable
    val mutable stack = []
    method push x = stack <- x :: stack
    method pop () = match stack with
      | x :: xs -> stack <- xs; x
      | [] -> raise (Failure "empty stack")
 end ;;

let main () =
  let stack = int_stack in
  stack#push true; stack#pop () ;;
