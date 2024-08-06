(*This benchmark tests the algorithm's ability to detect and report a complex type error that involves multiple levels of data flow mismatches, including a recursive function, a higher-order function, and a combination of different types.*)

let rec fib n =
  if n < 2 then n else fib (n - 1) + fib (n - 2);;

let combine f x y = (f x, f y);;

let main () = combine fib true 42;;
