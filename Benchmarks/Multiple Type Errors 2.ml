(* example 1 - tuple field types mismatch *)
let parse_version (s: string): string = 
  (* Dummy implementation that just returns the input string *)
  s;;

let show_major (s: string): string =
  (* Dummy implementation that returns a string indicating major version *)
  "Major version: " ^ s;;

let appInfo = ("My Application", 1.5);;

let process (name, vers) = name ^ show_major (parse_version vers);;

5;appInfo;;
let x = 5;;

let test = process appInfo;;
let test = process ("My Application", "1.5");;
print_int 5;;
let test = process ("My Application", "1.5");;
let test = process appInfo2;;
print_int x;;

let () = print_int 5;;

if x < 4 then
  let () = print_int 5 
  print_string "hello"
else
  print_string "world";;


