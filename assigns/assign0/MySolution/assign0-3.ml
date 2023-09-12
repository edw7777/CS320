(* ****** ****** *)
(* ****** ****** *)

let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0
;;
(* ****** ****** *)

let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0
;;
(* ****** ****** *)
(*
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)
let concat_helper(index: int) (s1: string) (s2: string): char =
  if index <= string_length(s1) - 1 then string_get(s1, index) else string_get(s2, index - string_length(s1))

let string_concatenate (s1: string) (s2: string): string =
  string_init(string_length(s1) + string_length(s2)) (fun i -> concat_helper(i)(s1)(s2))

let rec helper(i0: int):string = 
  if i0 = 0 then str(chr(i0 mod 10 +48)) 
  else string_concatenate(helper(i0 / 10)) (str(chr(i0 mod 10 +48)))

let rec int2str (i0: int): string =
  if i0 < 0 then string_concatenate("-") (helper(-1*i0)) else helper(i0)
  
let test = int2str (-1234567);;

(* ****** ****** *)