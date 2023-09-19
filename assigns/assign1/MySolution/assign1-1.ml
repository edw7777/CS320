(*
assign1-1: 10 points
Given a natural number n that is not a multiple
of 10, intrev10(n) is the natural number whose
representation in base 10 reverses that of the
number n.

fun intrev10(n: int): int

For instance, if n = 12345, then intrev10(n) is
54321; if n = 10203, then intrev10(n) is 30201.

Please give a TAIL-RECURSIVE implementation of
intrev10.
*)

let rec power(index: int) (holder: int): int =
  if index = 0 then holder else power(index - 1) (holder * 10)

let rec length(n: int)(holder: int): int =
  if n = 0 then holder else length (n / 10) (holder + 1)

let rec helper(n: int) (index: int) (holder: int): int= 
  if n < 10 then holder+n else helper(n / 10) (index - 1) (holder + (n mod 10) * power(index) (1))

let intrev10(n: int): int = 
  if n = 0 then 0 else helper(n) (length(n) (0)-1) (0)

let test = intrev10(1123458);;
