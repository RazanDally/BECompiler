(*This benchmark tests the algorithm's ability to handle modules and detect a type error when a module function is applied to arguments of an incompatible type*)

module IntUtils = struct
  let add x y = x + y
end ;;

module StringUtils = struct
  let concat x y = x ^ y
end ;;

let main () =
  let add = IntUtils.add in
  add "hello" "world" ;;
