let chr = Char.chr
let ord = Char.code
let str(c0) = String.make 1 c0

let string_init = String.init
let string_length = String.length
let string_get(cs, i0) = String.get cs i0

(*
Assign0-4: 10 points
Please implement a function that converts a given
string to an integer:
fun str2int(cs: string): int
In particular, it is expected that str2int(int2str(x)) = x
*)
let rec exponent(exp: int): int = 
  if exp > 0 then 10*exponent(exp-1) else 1

let rec helper(cs:string) (i: int): int =
  let length = string_length(cs) in
  if i < length then (ord(string_get(cs, i))-48) * exponent(length - 1 - i) + helper(cs) (i+1) else 0

let helper_substring(cs: string) (index: int): char = 
  string_get(cs, index)

let substring(cs:string): string =
  string_init(string_length(cs)-1)(fun i -> helper_substring(cs) (i+1))

let str2int(cs:string): int = 
  if string_get(cs, 0) = '-' then -1 * helper(substring(cs)) (0) else helper(cs)(0)
  
let test = str2int("-123");;
(* ****** ****** *) 