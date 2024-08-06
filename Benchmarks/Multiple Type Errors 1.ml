(*This program processes two lists, one of integers and one of strings, using functions that map and sum the lists. The process_lists function tries to add the results of summing these lists, leading to multiple type errors.*)
let rec map_list f lst =
  match lst with
  | [] -> []
  | x :: xs -> (f x) :: (map_list f xs) ;;

let rec sum_list lst =
  match lst with
  | [] -> 0
  | x :: xs -> x + (sum_list xs) ;;

let process_lists lst1 lst2 =
  let mapped_lst1 = map_list (fun x -> x + 1) lst1 in
  let mapped_lst2 = map_list (fun x -> x ^ "!") lst2 in
  sum_list mapped_lst1 + sum_list mapped_lst2 ;;

let main () =
  let int_list = [1; 2; 3] in
  let string_list = ["a"; "b"; "c"] in
  process_lists int_list string_list ;;
