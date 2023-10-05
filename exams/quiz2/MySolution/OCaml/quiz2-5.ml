(* ************************************************ *)
#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(*
Q2-5: 5 points
The function list_last returns the last element of a given
list. Please give a NON-RECURSIVE implementation of list_last
based on pattern matching and list_foldright. If the given list
is empty, raise the Empty exception
*)

(* ************************************************ *)

exception Empty
let list_length(xs) = 
  list_foldleft(xs) (0) (fun(res)(index) -> res + 1)

let list_last(xs: 'a list): 'a = 
  list_foldright(xs) (0, list_length(xs) - 1) (fun(res, iter_index)(element) -> if list_length(xs) - 1 == iter_index then (acc+element, iter_index-1) else (acc, iter_index-1))