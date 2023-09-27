#use "./../../../classlib/OCaml/MyOCaml.ml";;

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
string_sepjoin_list("")(["1";"22";"333"] = "122333"
For instance,
string_sepjoin_list(",")(["1";"22";"333"]) = "1,22,333"
For instance,
string_sepjoin_list(";;")(["11";"22";"33"]) = "11;;22;;33"
*)
let concat_helper(index: int) (s1: string) (s2: string): char =
  if index < string_length(s1) - 1 then string_get_at (s1) (index) else string_get_at(s2) (index - string_length(s1))

let string_concatenate (s1: string) (s2: string): string =
  string_init(string_length(s1) + string_length(s2)) (fun i -> concat_helper(i)(s1)(s2))

(*let rec helper (sep:string) (xs:string list) (index:int) (output:string) = 
  if index <= string_length(sep) - 1 then helper(sep) (xs) (index+1) (string_concatenate (string_concatenate(output) (index::xs) ) (sep) ) 
  else output*)

let rec string_sepjoin_list (sep: string) (xs: string list): string =
  match xs with
  | [] -> ""
  | hd :: [] -> hd
  | hd :: tl -> string_concatenate hd (string_concatenate sep (string_sepjoin_list sep tl))

let test2 = string_sepjoin_list(",")(["1";"22";"333"]);;

(* ****** ****** *)