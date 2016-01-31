(* -- Part A -------------------------------------------------------------- *)
grade_only [1; 2; 3; 4; 5 ; 6 ; 7; 8 ; 9 ; 10 ; 11 ; 12 ; 13 ; 14] ;;

type ltable = (string * string list) list
type distribution =
  { total : int ;
    amounts : (string * int) list }
type htable = (string, distribution) Hashtbl.t
type ptable =
  { prefix_length : int ;
    table : (string list, distribution) Hashtbl.t }

let rec words str =
  let sep = ' ' in
  try
    let i = String.index str sep in
    String.sub str 0 i ::
    words (String.sub str (i+1) (String.length str - i - 1))
  with Not_found ->
    [str]

let update_table x y table =
  try
    let pair = List.assoc x table in
    let lst = List.remove_assoc x table in
    (x, y::pair)::lst
  with Not_found ->
    (x, [y])::table;;

let build_ltable words =    
  let rec aux_build table l =
    match l with
    | [] -> table
    | x::[] -> aux_build (update_table x "STOP" table) []
    | x::y::xs -> aux_build (update_table x y table) (List.tl l) in
  aux_build ["START", [List.hd words]] words;;

let next_in_ltable table word =
  let lst = List.assoc word table in let i = Random.int (List.length lst) in
  List.nth lst i;;

let walk_ltable table =
  let v = List.assoc "START" table in
  let lst = (List.hd v)::[] in
  let rec aux word l =
    let s = next_in_ltable table word in 
    if s = "STOP" then (List.rev l)
    else aux s (s::l)
  in
  aux (List.hd lst) lst;;
(* -- Part B -------------------------------------------------------------- *)

let compute_distribution' l =
  let rec aux acc lst =
    match lst with
    | [] -> acc
    | x::xs -> let (l1, l2) = List.partition (fun e -> e = x) lst in aux ((x, (List.length l1))::acc) l2 
	 in
	 aux [] l;;

let compute_distribution l =
  { total = (List.length l) ; amounts = (compute_distribution' (List.sort compare l))};;

let update_table x y table =
  if Hashtbl.mem table x then 
    let lst = Hashtbl.find table x in
    Hashtbl.replace table x (y::lst);
  else Hashtbl.add table x [y];
  table;;

let create_table l =
  let ht = Hashtbl.create 40 in
  Hashtbl.add ht "START" [(List.hd l)];
  ht;;

let build_htable'  words =     
  let rec aux_build table l =
    match l with
    | [] -> table
    | x::[] -> aux_build (update_table x "STOP" table) []
    | x::y::xs -> aux_build (update_table x y table) (List.tl l) in
  aux_build (create_table words) words;;

let build_htable words =
  let ht = Hashtbl.create 0 in
  Hashtbl.iter (fun k v -> Hashtbl.add ht k (compute_distribution v)) (build_htable' words);
  ht;;

let build_list_from_tuple t =
  let res = ref [] in
  let s, n = t in
  for i = 0 to (n - 1) do
    res := s :: !res
  done;
  !res;;

let get_flat_list list_of_tuples =
  let rec aux lst ll = 
    match lst with
    | [] -> ll
    | x::xs ->  aux xs ((build_list_from_tuple x) @ ll) in
  aux list_of_tuples [];;  

let next_in_htable table word =
  let distr = (Hashtbl.find table word) in let total = distr.total in let amounts = distr.amounts in
  List.nth (get_flat_list amounts) (Random.int total);;


let walk_htable table =
  let a = next_in_htable table "START" in
  let lst = a::[] in
  let rec aux word l =
    let s = next_in_htable table word in 
    if s = "STOP" then (List.rev l)
    else aux s (s::l)
  in
  aux (List.hd lst) lst;;

(* -- Part C -------------------------------------------------------------- *)
type char_end = End_sentence | Empty_sentence | Continue | Word | Separator;;

let terminate_char str =
  if String.length str > 1 then false
  else if String.get str (String.length str - 1) = '?' then true 
  else if String.get str (String.length str - 1) = '!' then true 
  else if String.get str (String.length str - 1) = '.' then true 
  else false;;
  
let parse char =
  match char with
  | '.' | '?' | '!'                    -> End_sentence
  | '\n'                               -> Empty_sentence
  | 'a'..'z' | 'A'..'Z' | '0'..'9'     -> Continue
  | '\128'..'\255'                     -> Continue
  | ';' | ',' | ':' | '-' | '"' | '\'' -> Word
  | _                                  -> Separator
  
let append_str str ch =
  let s = String.make 1 ch in str ^ s;;
  
let sentences' str  =
  let l = ref [] in
  let this_word = ref "" in
  for i = 0 to (String.length str - 1) do
    let ch = String.get str i in
    let res = parse ch in
    match res with
    | End_sentence | Word ->  if (String.length !this_word) > 0 then 
          begin
            l := !this_word :: !l; this_word := ""; this_word := append_str !this_word ch; l := !this_word :: !l; this_word := ""
          end
        else
          begin
            this_word := append_str !this_word ch; l := !this_word :: !l; this_word := ""
          end
    | Continue -> this_word := append_str !this_word ch
    | Separator | Empty_sentence -> if (String.length !this_word) > 0 then  l := !this_word :: !l; this_word := ""
  done;
  if (String.length !this_word) > 0 then  l := !this_word :: !l;
  (List.rev !l);;
  
let sentences str =
  let lst = sentences' str in
  let rec aux_sent l acc ll = 
    match l with
    | [] -> if (List.length ll) > 0 then (List.rev ll) else (List.rev (lst::ll))
    | x::[] -> if (terminate_char x) = true then aux_sent [] [] ((List.rev (x::acc))::ll) else aux_sent [] [] ((List.rev (x::acc))::ll) 
    | x::xs -> if (terminate_char x) = true then aux_sent xs [] ((List.rev (x::acc))::ll) else aux_sent xs (x::acc) ll in
  aux_sent lst [] [];;
let rec start lp =
  let rec aux_start l i =
    if i <=  0 then l else aux_start ("START"::l) (i - 1) in
  aux_start [] lp;;

let shift l x =
  List.rev (x::(List.rev (List.tl l)));;

let create_temp_table header x =
  let ht = Hashtbl.create 0 in
  Hashtbl.add ht header [x];
  ht;;

let build_ptable'  words lp =     
  let rec aux_build table header l =
    match l with
    | [] -> table
    | x::[] -> aux_build (update_table (shift header x) "STOP" table)(shift header x) []
    | x::y::xs -> aux_build (update_table (shift header x) y table) (shift header x) (List.tl l) in
  aux_build (create_temp_table (start lp) (List.hd words)) (start lp) words;;  

let build_ptable words lp =
  let ht = Hashtbl.create 0 in
  Hashtbl.iter (fun k v -> Hashtbl.add ht k (compute_distribution v)) (build_ptable' words lp);
  { prefix_length = lp ; table = ht};;

let walk_ptable ptable =
  let prefix_length = ptable.prefix_length in
  let table = ptable.table in
  let header = start prefix_length in
  let a = next_in_htable table header in
  let lst = a::[] in
  let rec aux keyword word l =
    let s = next_in_htable table (shift keyword word) in 
    if s = "STOP" then (List.rev l)
    else aux (shift keyword word) s (s::l)
  in
  aux header (List.hd lst) lst;;

let list_make str n =
  let l = [] in
  let rec aux_list_make i ll =
    match i with
    | 0 -> ll
    | x -> aux_list_make (i - 1) (str::ll) in
  aux_list_make n l;;

let tuple_list_to_list t = List.fold_left (fun l (s, i) -> (list_make s i)@l) [] t;;

let hashtbl_to_list h = Hashtbl.fold (fun k v acc -> (k, (tuple_list_to_list v.amounts)) :: acc) h [];;

let update_table_ex x y table =
  if Hashtbl.mem table x then 
    let lst = Hashtbl.find table x in
    Hashtbl.replace table x (y@lst);
  else Hashtbl.add table x y;
  table;;
  
let build_table_ex lst =
  let rec aux table l =
    match l with
    | [] -> table
    | (k, v)::xs -> aux (update_table_ex k v table) xs in
  aux (Hashtbl.create 0) lst;;
  
let merge_ptables'' tl =
  let rec aux tlist lst =
    match tlist with
    | [] -> lst
    | x::xs -> aux xs (x.table :: lst) in
  aux tl [];;
  
let merge_ptables' tl = 
  let v = merge_ptables'' tl in
  List.fold_left (fun l h -> (hashtbl_to_list h)@l) [] v;;
  
let merge_ptables_ex tl =
  let v = merge_ptables' tl in
  build_table_ex v;;
  
let merge_ptables tl =
  let pl = (List.hd tl).prefix_length in
  let htl = merge_ptables_ex tl in
  let ht = Hashtbl.create 0 in
  Hashtbl.iter (fun k v -> Hashtbl.add ht k (compute_distribution v)) htl;
  { prefix_length = pl ; table = ht};;
