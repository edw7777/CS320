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
Assign0-5: 10 points
Please implement a function that returns the reverse of
a given string:
fun stringrev(cs: string): string
*)
let concat_helper(index: int) (s1: string) (s2: string): char =
  if index <= string_length(s1) - 1 then string_get(s1, index) else string_get(s2, index - string_length(s1))

let string_concatenate (s1: string) (s2: string): string =
  string_init(string_length(s1) + string_length(s2)) (fun i -> concat_helper(i)(s1)(s2))

let rec stringhelper(cs:string) (index:int): string =
  if string_length(cs) - index -1 = 0 then str(string_get(cs, 0)) else
    string_concatenate(str(string_get(cs, (string_length(cs)-1-index)))) (stringhelper(cs) (index + 1))

let stringrev(cs: string): string =
  if string_length(cs) > 0 then stringhelper(cs) (0) else cs

let test = stringrev("Mensun");;

(* ****** ****** *)