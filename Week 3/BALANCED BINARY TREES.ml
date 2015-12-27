(*BALANCED BINARY TREES  (22/22 points)
A binary tree t, of the 'a bt type given in the prelude, is either an empty tree, or the root of a tree with a value and two children subtrees.

Write a function height : 'a bt -> int that computes the height of a tree.
A tree is balanced if, for all internal node n, its two subtrees have the same height. Write a function balanced : 'a bt -> bool that tells if a tree is balanced.
THE GIVEN PRELUDE
*)

type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt ;;
  
let rec height t =
  match t with
  | Empty -> 0
  | Node(l,  y, r) -> let lh = 1 + (height l) in let rh = 1 + (height r) in max lh rh;;

let rec balanced t  = 
  match t with
  | Empty -> true
  | Node(l, y, r) -> if (height l) <> (height r) then false else balanced l && balanced r;;
