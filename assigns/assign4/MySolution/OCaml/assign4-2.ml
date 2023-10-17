#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(*
//
Assign4-2:
//
HX-2023-10-05: 10 points
//
Please enumerate all the pairs of natural
numbers. Given pairs (i1, j1) and (i2, j2),
(i1, j1) should be enumerated ahead of (i2, j2)
if i1+j1 < i2+j2.
//
let theNatPairs: (int*int) stream = fn () => ...
//
*)

let first : int stream = 
  let rec lazy_loop i curr_index =  
    if curr_index < i then
      fun () -> StrCons (curr_index, lazy_loop (i) (curr_index+1))
    else 
      fun () -> StrCons (i, lazy_loop (i+1) (0))
  in lazy_loop 0 0 

let second : int stream =
  let rec lazy_loop i curr_index = 
    if curr_index > 0 then
      fun () -> StrCons (curr_index, lazy_loop (i) (curr_index-1))
    else
      fun () -> StrCons (0, lazy_loop (i+1) (i+1))
  in lazy_loop 0 0 

let rec stream_zip (firstnum : 'a stream) (secondnum: 'b stream) : ('a * 'b) stream =
  fun() ->
  match first (), second () with
  | StrNil, _ -> StrNil
  | _, StrNil -> StrNil
  | StrCons (hd1, tl1), StrCons(hd2, tl2) -> StrCons((hd1, hd2), stream_zip tl1 tl2)

let theNatPairs: (int*int) stream = 
  let firstnum = first in
  let secondnum = second in
  stream_zip (firstnum)(secondnum)
  ;;

  
(*let StrCons(x1, xs) = first();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();;
let StrCons(x1, xs) = xs();; *)