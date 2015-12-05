(* ROTATING THE CONTENTS OF AN ARRAY  (22/22 points)
In this exercise, you will improve the code shown in the course (and given in the template) for rotating arrays.

There is something perfectible with the code of rotate. 
Find what, and fix the function!.
Define rotate_by: 'a array -> int -> unit adding a parameter that allows to rotate by n positions. 
For instance, rotate_by [|1;2;3;4|] 3 should yield [|4;1;2;3|].
*)

let rotate a =
  let n = Array.length a in 
  if n > 0 then
    begin
      let v = a.(0) in
      for i = 0 to n-2 do
        a.(i) <- a.(i+1)
      done;
      a.(n-1)<-v ;
    end;;

let rotate_by a n =
  if n >= 0 then
    begin
      for i = 0 to n-1 do
        rotate a
      done;
    end
  else let k = (Array.length a) - abs (n mod (Array.length a)) in
    begin 
      for i = 0 to k -1 do
        rotate a
      done;
    end;;