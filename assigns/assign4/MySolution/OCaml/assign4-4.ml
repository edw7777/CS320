#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(*
//
Assign4-4:
//
HX-2023-10-05: 20 points
//
Please enumerate all the permuations of a
given list. The enumeration is required to be
in order. For instance, say xs = [1;2;3], then
the enumeration should be of the following order
[1;2;3], [1;3;2], [2;1;3], [2;3;1], [3;1;2], [3;2;1].
//
let list_permute(xs: 'a list): 'a list stream
*)

(*let list_permute(xs: 'a list): 'a list stream = 
  list_foldleft(xs) ([[]]) (fun(res)(first_element) -> first_element :: ) *)

(* let rec insert_all_positions x lst =
  let rec insert_at x lst pos =
    match pos, lst with
    | 0, _ -> x :: lst
    | _, [] -> [x]
    | _, hd :: tl -> hd :: insert_at x tl (pos - 1)
  in
  let len = List.length lst in
  List.init (len + 1) (insert_at x lst)

let rec list_permute (xs: 'a list): 'a list stream =
  let rec permute_aux lst =
    match lst with
    | [] -> StrNil
    | [x] -> StrCons (x, fun () -> StrNil)
    | _ ->
      let fxs () =
        let insertions = insert_all_positions (List.hd lst) (List.tl lst) in
        let permute_streams = List.map (fun x -> list_permute (x @ List.tl lst)) insertions in
        stream_map (fun () -> List.hd permute_streams ()) (fun x -> x) ()
      in
      StrCons (List.hd lst, fxs)
  in
  fun () -> permute_aux xs

  *)
(* ****** ****** *)
