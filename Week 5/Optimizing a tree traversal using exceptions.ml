(*
OPTIMISING A TREE TRAVERSAL USING EXCEPTIONS  (22/44 points)
In a previous exercise, we defined binary trees of type 'a bt (given in the prelude). 
A binary search tree is balanced if, for all internal node n, its two subtrees have the same height.

You wrote then a function height for balanced trees, and a function balanced that checks whether a tree is balanced, that looked like the ones given in the template. 
This code is quite elegant, but not the most efficient. In this exercise we will improve it using exceptions.

1 Without changing their algorithms, instrument height and balanced to make them return also the number of visits of leaves (each encounter of an Empty constructor counts for 1, Nodes are not counted). The functions should now be typed height : 'a bt -> int * int and balanced : 'a bt -> bool * int where the right part of the result pair is the number of visits. 
When rewriting balanced, don't forget to include the results of all the calls to height in the total sum. You should also remember that if the left hand side of a (&&) is false, the rest is not executed, so be cautious in your rewriting to have the same behaviour (adding ifs if necessary), otherwise you could end up calling height more than the original version.


2 We now define an exception Unbalanced of int, that we use in a function bal_height : 'a bt -> int * int that returns the height of a balanced tree, with the number of visits of leaves and raises Unbalanced n if the input tree is not balanced. The exception must be raised as soon as possible, when the first pair of subtrees with different heights is encountered. The parameter n of the exception is the number of visits of leaves up to the exception. You may have to define an auxiliary recursive function that takes an additional parameter containing the number of visits already done during the start of the traversal.

3 Define now an improved version balanced_fast : 'a bt -> bool * int, that uses bal_height and returns a boolean indicating whether the input tree is balanced, and the number of visits.
THE GIVEN PRELUDE
*)

type 'a bt =
  | Empty
  | Node of 'a bt * 'a * 'a bt ;;

exception Unbalanced of int ;;

let rec height' t =
  match t with
  | Empty -> 0
  | Node(l,  y, r) -> 1 + (max(height' l) (height' r));;

let rec lcount t =
  match t with
  | Empty -> 1
  | Node(l,  y, r) -> lcount l + lcount r;;

let rec height l =
  ((height' l),(lcount l));;

let rec balanced = function
    Empty -> (true, 1)
  | Node (l, _, r) -> let (lb, lc) = balanced l in
      if lb then
        let (rb, rc) = balanced r in
        if rb then
          let (lh, lc') = height l in
          let (rh, rc') = height r in (lh = rh, lc + rc + lc' + rc')
        else (false, lc + rc)
      else (false, lc);;
	  
let bal_height bst =
  let rec aux h c = function
    | Empty -> (h, c + 1) 
    | Node (t,_,t') -> match ((aux (h + 1) c t), (aux (h + 1) c t')) with
        ((h1, c1),(h2,c2)) ->
          if h1 <> h2 then raise (Unbalanced (c1 + c2))
          else
            ((h1),(c1 + c2))
  in aux 0 0 bst;;
  
let balanced_fast bst =
  let (h, c) = try bal_height bst with Unbalanced n -> (-1, n) in
  match (h, c) with
  | -1, x -> (false, x)
  | _, x -> (true, x);;