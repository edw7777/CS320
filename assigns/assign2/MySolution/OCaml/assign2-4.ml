(*
//
Assign2-4: 10 points
//
Please given a combinator-based implementation of
string_sepjoin_list:
let
string_sepjoin_list
(sep: string)(xs: string list): string = ...

For instance,
string_sepjoin_list("")(["1";"22";"333"]) = "122333"
For instance,
string_sepjoin_list(",")(["1";"22";"333"]) = "1,22,333"
For instance,
string_sepjoin_list(";;")(["11";"22";"33"]) = "11;;22;;33"
*)

let string_sepjoin_list (sep: string) (xs: string list): string =
  let combine acc x = acc ^ sep ^ x in
  match xs with
  | [] -> "" (* Handle the case of an empty list *)
  | hd :: tl -> List.fold_left combine hd tl (* Fold the list with the combine function *)

let test1 = string_sepjoin_list "" ["1"; "22"; "333"] (* "122333" *)

(* ****** ****** *)