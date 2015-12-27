(*THE OPTION TYPE  (30/30 points)
Optional values are commonly used in OCaml in the return type of partial functions, i.e. functions that may fail on some input. The following questions illustrate such situations. 
In the Pervasives module which is loaded automatically, there is a type option with two constructors: 
Some (e) has type 't option if e has type 't and represents the presence of some value e of type 't. 
None has type 't option and represents the absence of some value of type 't.

Write a function find : string array -> string -> int option such that find a w = Some idx if a.(idx) = w and find a w = None if there is no such index.
Sometimes, when a value of type t is missing, a default value should be used. 
Write a function default_int : int option -> int such that: default_int None = 0 and default_int (Some x) = x.
Write a function merge : int option -> int option -> int option such that:
merge None None = None
merge (Some x) None = merge None (Some x) = Some x
merge (Some x) (Some y) = Some (x + y)
*)

let rec find_index a w i =    
  if i = -1 then -1
  else if String.compare a.(i) w = 0 then i
  else find_index a w (i-1);;


let find a w =
  let f = find_index a w (Array.length a - 1) in
  match f with
  | -1 -> None
  | x  -> Some x;;

let default_int w =
  match w with
  | None -> 0
  | Some w -> w;;

let merge a b =
  match a with
  | None -> (match b with 
      | None -> None
      | Some x -> Some x )
  | Some y -> (match b with
      | None -> Some y
      | Some z -> Some (y + z) );;

