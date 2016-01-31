(* 
SIMPLE USES OF REFERENCES  (50/50 points)
1 Define swap : 'a ref -> 'a ref -> unit that swaps the contents of two references.
2 Define update : 'a ref -> ('a -> 'a) -> 'a that calls a function on the contents of a reference, updates it with the result, and returns the old value. 
For instance let r = ref 6 in update r (function x -> x + 1) should return 6 and set the reference to 7.
3 Define move: 'a list ref -> 'a list ref -> unit, that removes the top argument from the first list and puts it on top of the second list. If the first list is empty, it should raise Empty.
A common pattern is to use a reference to perform a computation in an imperative way, but to keep it in a local definition, completely invisible from outside the function implementation. 
4 Define reverse: 'a list -> 'a list, that has a type and an observable behaviour that look like the ones of a purely functional function, buf that use a reference internally to perform the computation. It takes a list, and returns a copy of the list whose elements are in reverse order. 
The only functions you can call, except from locally defined functions, are (!), (:=), ref, and move that you just defined. And you are not allowed to use pattern matching.
THE GIVEN PRELUDE*)

exception Empty ;;

let swap ra rb =
  begin
    let c = !ra in ra := !rb; rb := c 
  end;;

let update r f =
  begin
    let c = !r in r := f c;
    c
  end;;
    

let move l1 l2 =
  let head = try List.hd !l1 with exn -> raise Empty in
  l1 := List.tl !l1;
  l2 := head:: !l2;;


let reverse l =
  let lc = ref [] in
  let ll = ref [] in ll := l;
  let docopy() =
    try
      while true do 
        move ll lc
      done
    with _ -> ()
  in
  docopy();
  !lc;;
  

