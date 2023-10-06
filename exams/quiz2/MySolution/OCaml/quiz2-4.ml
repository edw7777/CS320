#use "./../../../../classlib/OCaml/MyOCaml.ml";;
(* ************************************************ *)

(*
Q2-4: 5 points
The function list_last returns the last element of a given
list. Please give a NON-RECURSIVE implementation of list_last
based on pattern matching and list_foldleft. If the given list
is empty, raise the Empty exception
*)

(* ************************************************ *)
exception Empty 
let list_length(xs) = 
  list_foldleft(xs) (0) (fun(res)(index) -> res + 1)

  let list_last(xs: 'a list): 'a =
  match xs with
  | [] -> ""
  | x1 :: xs ->
    list_foldleft (xs) (x1, 0) (fun(res, iter_index)(element) -> if iter_index = (list_length(xs) - 1) then (element, iter_index) else (res, iter_index+1))

let test = list_last [a;b;c];;