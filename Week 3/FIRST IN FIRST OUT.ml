(*FIRST IN FIRST OUT  (50/50 points)
A queue is a standard FIFO data structure. See wikipedia

In this exercise, we implement a queue with a pair of two lists (front, back) such that front @ List.rev back represents the sequence of elements in the queue.

Write a function is_empty : queue -> bool such that is_empty q is true if and only if q has no element.
Write a function enqueue : int -> queue -> queue such that enqueue x q is the queue as q except that x is at the end of the queue.
Write a function split : int list -> int list * int list such that split l = (front, back) where l = back @ List.rev front and the length of back and front is List.length l / 2 or List.length l / 2 + 1
Write a function dequeue : queue -> int * queue such that dequeue q = (x, q') where x is the front element of the queue q and q' corresponds to remaining elements. This function assumes that q is non empty.
THE GIVEN PRELUDE
*)

type queue = int list * int list

let is_empty (front, back) =
  let q = front @ List.rev back in
  match q with
  | [] -> true
  | _ -> false;;

let enqueue x (front, back) =
  (front, x::back);;

let rec rev = function
  | [] -> []
  | x :: xs -> rev xs @ [ x ];;


let split l =
  let rec split1 (l1, l2) accu = 
    match (l1, l2) with
    | xs, ([] | [_]) -> (rev accu, rev xs)
    | [], _ -> ([], [])
    | x::xs, y::y'::ys -> let t = x::accu in split1 (xs, ys) t in
  split1 ((List.rev l), (List.rev l)) [];;


let rec dequeue (front, back) =
  match front with
  | x::xs -> (x,(xs,back))
  | _     -> dequeue (List.rev back, []);;
