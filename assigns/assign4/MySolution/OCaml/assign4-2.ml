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


(*let rec leftNum(index: int) (specIndex: int) (maxindex:int): int stream =
  fun() ->
    if (specIndex = 0) then (StrCons(0, leftNum(0)(specIndex+1)(maxindex)))
    else if (specIndex = 1) then (StrCons(0, leftNum(0)(specIndex+1)(maxindex)))
    else if (specIndex = 2) then (StrCons(1, leftNum(0)(specIndex+1)(maxindex)))
    else (
      if (index < maxIndex) then (StrCons(index, leftNum(index+1)(specIndex)(maxindex)))
      else (StrCons(index, leftNum(0)(specIndex)(maxindex+1)))
    )

let rec rightNum(index: int) (specIndex: int) (maxindex:int): int stream =
  fun() ->
    if (specIndex = 0) then (StrCons(0, rightNum(index)(specIndex+1)(maxindex)))
    else if (specIndex = 1) then (StrCons(1, rightNum(index)(specIndex+1)(maxindex)))
    else if (specIndex = 2) then (StrCons(0, rightNum(index)(specIndex+1)(maxindex)))
    else (
      if (index > 0) then (StrCons(index, rightNum(index-1)(specIndex)(maxindex)))
      else (StrCons(index, rightNum(maxindex+1)(specIndex)(maxindex+1)))
    )
    *)

let rec stream_zip (firstnum : 'a stream) (secondnum: 'b stream) : ('a * 'b) stream =
  fun() ->
  match firstnum(), secondnum() with
  | StrNil, _ -> StrNil
  | _, StrNil -> StrNil
  | StrCons (hd1, tl1), StrCons(hd2, tl2) -> StrCons((hd1, hd2), stream_zip tl1 tl2)

let theNatPairs: (int*int) stream = 
  let firstnum = first in
  let secondnum = second in
  stream_zip (firstnum)(secondnum)

(*let theNatPairs: (int*int) stream = 
    let first = leftNum(0)(0)(2) in
    let second = rightNum(2)(0)(2) in
    stream_zip(first)(second) *)


let rec stream_listsize(n : int)( st: 'a stream) : 'a list=
  if n <= 0 then []
  else 
    match st () with
    | StrNil -> []
    | StrCons (hd, tl) ->
      hd :: stream_listsize (n - 1) tl
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