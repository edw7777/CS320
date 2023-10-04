#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(*
Assign3-3:
HX-2023-09-26: 10 points
//
The function [list_nchoose(xs)(n0)]
returns all the subsequences of xs that are
of length n0.
//
let rec
list_nchoose
(xs: 'a list)(n0: int): 'a list list =
//
Please give a NON-RECURSIVE implementation of
list_nchoose based on list-combinators. Note that
the order of the elements in a list representation
of a subsequenc is SIGNIFICANT. For instance, [1;2]
and [2;1] are DIFFERENT.
//
*)
let list_size(xs: 'a list): int =
  list_foldleft(xs) (0) (fun(res)(x1) -> 1 + res)

let list_subsets (xs: 'a list) : 'a list list =
  list_foldleft(xs) ([[]]) (fun(res)(x1) -> list_foldleft(res)(res)(fun(res)(x2) -> (x1 :: x2) :: res))
  
(* if Array.length(xs) == n0*)
let list_nchoose (xs: 'a list) (n0: int): 'a list list =
  (*(fun work -> list_foreach(list_subsets(xs)) (fun(xs)))*)
  list_foldleft(list_subsets(xs))([]) (fun(res)(x1) -> if list_size(x1) == n0 then x1::res else res)

let test = list_nchoose( [1;2;3] ) (2);;
