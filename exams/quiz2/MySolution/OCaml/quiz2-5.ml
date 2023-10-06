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

let list_length(xs) = 
  list_foldleft(xs) (0) (fun(res)(index) -> res + 1)

let list_last(xs: 'a list): 'a = 
  match xs with
  | [] -> raise exception Empty
  | let (output , _)= list_foldright(xs) (0, list_length(xs) - 1) (fun(res, iter_index)(element) -> if list_length(xs) - 1 == iter_index then (res+element, iter_index-1) else (res, iter_index-1))

let test = list_last([1;2;3]);;