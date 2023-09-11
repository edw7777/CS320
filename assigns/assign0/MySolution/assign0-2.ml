(*
Assign0-2: 10 points
Please implement a function that tests whether a
given natural number is a prime:
fun isPrime(n0: int): bool
*)

let rec isdivisble (n0: int) (y: int): bool =
  if y <= n0/2 then (n0 mod y == 0 || isdivisble(n0) (y+1)) else false

let isPrime(n0: int): bool = 
  if isdivisble(n0)(2) then false else if (n0 == 0) then false else true
    
let test = isPrime(5);;