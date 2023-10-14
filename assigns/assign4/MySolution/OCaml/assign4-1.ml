#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(*
//
Assign4-1:
//
HX-2023-10-05: 10 points
//
The following is a well-known series:
ln 2 = 1 - 1/2 + 1/3 - 1/4 + ...
Please implement a stream consisting of all the
partial sums of this series.
The 1st item in the stream equals 1
The 2nd item in the stream equals 1 - 1/2
The 3rd item in the stream equals 1 - 1/2 + 1/3
The 4th item in the stream equals 1 - 1/2 + 1/3 - 1/4
And so on, and so forth
//
let the_ln2_stream: real stream = fun() -> ...
//
*)

let the_ln2_stream: float stream = 
  let rec helper n index even=
    if even then 
      fun() -> StrCons (n, helper (n +. ( 1.0/.(index+.1.0) ) ) (index+. 1.0) (not even) ) 
    else 
      fun() -> StrCons (n, helper (n -. ( 1.0/.(index+.1.0) ) ) (index+. 1.0) (not even ) )
  in helper 1.0 1.0 false


(* ****** ****** *)