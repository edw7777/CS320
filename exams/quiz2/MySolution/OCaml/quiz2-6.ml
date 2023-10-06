#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(* ************************************************ *)

(*
Q2-6: 10 points

The function list_reverse return the reverse of a given list.
Please give an implementation of list_reverse based on list_foldright
(not list_foldleft).
*)

(* ************************************************ *)
(*let list_reverse(xs: 'a list): 'a list =
  list_foldleft(xs) ([]) (fun(res)(element) -> element::res) *)


let list_reverse (xs: 'a list): 'a list =
  let append_element x acc = acc @ [x] in
  list_foldright xs [] append_element

let test = list_reverse([a;b;c]);;
