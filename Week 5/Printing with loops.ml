(*
PRINTING WITH LOOPS  (105 points possible)
1 Using the for loop construct, write an output_multiples : int -> int -> int -> unit function that prints all the multiples of x in the integer interval n ... m, separated by commas (',').

2 Define a higher order function display_sign_until_zero: (int -> int) -> int -> unit that takes a function f, an integer m and applies f from 0 to m using a for loop. 
The function will print "negative" if the result of f if strictly negative and "positive" if strictly positive. Each print should appear on a new line. 
Your function has to stop displaying the signs as soon as f returns 0. In this case, it must print a last "zero". 
To implement this, you will define your own exception, raise it from inside the loop to break it, and catch it outside of the loop so that the function returns a successful (). 
You cannot use a predefined exception.
THE GIVEN PRELUDE
*)

let is_multiple i x = i mod x = 0;;


let output_multiples x n m =
  for i = n to m do
    if (i mod x = 0)then
      begin print_int i; print_string ", ";
      end

  done;;

exception ZeroException of int;;
let display_sign_until_zero f m =
  try 
    for i = 0 to m do
      if (f i) > 0 then begin print_string "positive"; print_newline(); end
      else if (f i) < 0 then begin print_string "negative"; print_newline(); end
      else raise (ZeroException 0)
    done;
  with ZeroException x -> print_string "zero";;
