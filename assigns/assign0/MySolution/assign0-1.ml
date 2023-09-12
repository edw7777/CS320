(* ****** ****** *)
(*
Assign0: Warmup!
*)
(* ****** ****** *)

(*
fun fact(x: int): int =
  if x > 0 then x * fact(x-1) else 1
*)

(*
Assign0-1: 10 points
Please find the first integer N such that the
evaluation of fact(N) in OCaml yields an Overflow
exception.
*)

let rec factorial(n) = 
  if n > 0 then n * factorial(n-1) else 1

let rec loop(n) = 
  if factorial(n) > 0 then loop(n+1) else n

let myAns = loop(0);;

(* ****** ****** *)

(*
Assign0-2: 10 points
Please implement a function that tests whether a
given natural number is a prime:
fun isPrime(n0: int): bool
*)
   


(* ****** ****** *)

(*
Assign0-3: 10 points
Please implement a function that converts a given
integer to a string that represents the integer:
fun int2str(i0: int): string
*)


(* ****** ****** *)

(*
Assign0-4: 10 points
Please implement a function that converts a given
string to an integer:
fun str2int(cs: string): int
In particular, it is expected that str2int(int2str(x)) = x
*)

(* ****** ****** *)

(*
Assign0-5: 10 points
Please implement a function that returns the reverse of
a given string:
fun stringrev(cs: string): string
*)

(* ****** ****** *)

(* end of [CS320-2023-Fall-assign0.ml] *)
