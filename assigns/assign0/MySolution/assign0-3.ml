(* ****** ****** *)

(*
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)
let string_concatenate (s1: string) (s2:string): bool =


let int2str (i0: int): string =
  if i0 = 0 then str(chr(i0 mod 10)) 
  else int2str(i0 mod 10) ^ str(chr(i0 mod 10))

let test = int2str 17;;
(* ****** ****** *)