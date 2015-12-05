type stack = int array
exception Full
exception Empty

let create size =
  Array.make (size + 1) 0;;

let push buf elt =
  let n = buf.(0) in let l = Array.length buf in
  if n = (l -1) then raise Full
  else buf.(0) <- n + 1; buf.(n+1) <- elt;;
    

let append buf arr =
  let l = Array.length arr in
  if l > 0 then
    begin
      for i = l - 1 downto 0 do
        push buf arr.(i)
      done;
    end;;

let pop buf =
  if (Array.length buf) = 0 then raise Empty;
  let n = buf.(0) in
  if n = 0 then raise Empty
  else 
    begin
      buf.(0) <- (n -1);
      buf.(n);
    end;;
