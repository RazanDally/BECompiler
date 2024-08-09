(*This benchmark tests the algorithm's ability to handle higher-order functions and detect a type error
 when an incompatible argument type is passed to a function that expects a function as its argument.*)

 let twice f x = f (f x) ;;
let main () = twice (fun x -> x + 1) true ;;
//│ [ERROR] Type `bool` does not match `int`
//│ 
//│	(bool) ---> (?c)  ~~~~ (?a)  ~~~~ (?b)  ~~~~ (int)
//│ 
//│ ◉ (bool) comes from
//│ │  - l.5  let main () = twice (fun x -> x + 1) true ;;
//││                                              ^^^^
//│ ▼ 
//│ ◉ (?c)  is assumed for
//│    - l.4   let twice f x = f (f x) ;;
//│                       ^
//│   ◉ (?c  -> ?a) comes from
//│   ▲  - l.4   let twice f x = f (f x) ;;
//│  │                             ^
//│   │  - l.4   let twice f x = f (f x) ;;
//│  │                    ^
//│   │ 
//│   ◉ (?d)  is assumed for
//│   │  - l.4   let twice f x = f (f x) ;;
//│  │                    ^
//│   ▼ 
//│   ◉ (?a  -> ?b) comes from
//│      - l.4   let twice f x = f (f x) ;;
//│                             ^
//│ ◉ (?a)  is assumed for
//│    - l.4   let twice f x = f (f x) ;;
//│                             ^^^^^
//│   ◉ (?c  -> ?a) comes from
//│   ▲  - l.4   let twice f x = f (f x) ;;
//│  │                             ^
//│   │  - l.4   let twice f x = f (f x) ;;
//│  │                    ^
//│   │ 
//│   ◉ (?d)  is assumed for
//│   │  - l.4   let twice f x = f (f x) ;;
//│  │                    ^
//│   ▼ 
//│   ◉ (?a  -> ?b) comes from
//│      - l.4   let twice f x = f (f x) ;;
//│                             ^
//│ ◉ (?b)  is assumed for
//│    - l.4   let twice f x = f (f x) ;;
//│                           ^^^^^^^
//│   ◉ (?a  -> ?b) comes from
//│   ▲  - l.4   let twice f x = f (f x) ;;
//│  │                          ^
//│   │  - l.4   let twice f x = f (f x) ;;
//│  │                    ^
//│   │ 
//│   ◉ (?d)  is assumed for
//│   ▲  - l.4   let twice f x = f (f x) ;;
//│  │                    ^
//│   │ 
//│   ◉ (_ -> int) comes from
//│      - l.5  let main () = twice (fun x -> x + 1) true ;;
//│                                ^^^^^^^^^^^^^^^^
//│ ◉ (int) comes from
//│    - l.5  let main () = twice (fun x -> x + 1) true ;;
//│                                        ^^^^^
//│    - lib. let (^): string -> string -> string
//│                                  ^^^
//│ twice: ('a -> 'a) -> 'a -> 'a
//│ main: () -> 'a
//│   where
//│     'a = bool, int
//│ U max: 4, total: 28
//│ UERR 1 errors
//│ L: 1 [bool ~ int, bool <: α50', [α50' - ([α50'] -> α48') ~ (α48' -> α49') - α48', L: 0 [([α50'] -> α48') ~ (α48' -> α49'), ([α50'] -> α48') :> α47', α47' <: (α48' -> α49')]], [α48' - ([α50'] -> α48') ~ (α48' -> α49') - α49', L: 0 [([α50'] -> α48') ~ (α48' -> α49'), ([α50'] -> α48') :> α47', α47' <: (α48' -> α49')]], [α49' - (α48' -> α49') ~ (α51' -> [int]) - int, L: 0 [(α48' -> α49') ~ (α51' -> [int]), (α48' -> α49') :> α47', α47' :> (α51' -> [int])]]]