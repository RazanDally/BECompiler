(*This benchmark tests that the algorithm does not report any errors for a simple, well-typed program.*)
let parse_version (s: string): string = 
  (* Dummy implementation that just returns the input string *)
  s;;

let show_major (s: string): string =
  (* Dummy implementation that returns a string indicating major version *)
  "Major version: " ^ s;;

let appInfo = ("My Application", "1.5");;

let () = print_int 4;;

ignore (5);appInfo;;

let process (name, vers) = name ^ show_major (parse_version vers);;

let () = print_int (String.length (process appInfo));;

let () = print_int 5;;

let x = 5;;

if x < 4 then
  let () = print_int 5 in
  print_string "hello"
else
  print_string "world";;

