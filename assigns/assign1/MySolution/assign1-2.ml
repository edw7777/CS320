#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*
assign1-2: 10 points
Given two ordered strings cs1 and cs2, please use
string_make_fwork to implement string_merge(cs1, cs2)
which returns the order string obtained from merging
cs1 and cs2.

For instance, if cs1 = "135" and cs2 = "2468", then
string_merge(cs1)(cs2) equals "1234568"

For instance, if cs1 = "abcde" and cs2 = "1234", then
string_merge(cs1)(cs2) equals "1234abcde"
*)
let helper_substring(cs: string) (index: int): char = 
  string_get_at(cs) (index)

let substring(cs:string): string =
  string_init(string_length(cs)-1)(fun i -> helper_substring(cs) (i+1))

let rec helper(cs1:string) (cs2:string) (index: int) (totallen: int) (work: char -> unit): unit =
  if index > totallen then ()
  else if string_length(cs1) == 0 then (work(string_get_at(cs2)(0)) ; helper(cs1) (substring(cs2)) (index+1) (totallen) (work) ) else 
  if string_length(cs2) == 0 then (work(string_get_at(cs1)(0)) ; helper(substring(cs1)) (cs2) (index+1) (totallen) (work)) else 
  if ord(string_get_at(cs1)(0)) - 48 < ord(string_get_at(cs2)(0)) - 48 then (work(string_get_at(cs1)(0)) ; helper(substring(cs1)) (cs2) (index+1) (totallen) (work) )
  else (work(string_get_at(cs2)(0)) ; helper(cs1) (substring(cs2)) (index+1) (totallen) (work))

let string_merge(cs1: string) (cs2: string): string =
  string_make_fwork (fun work ->(helper (cs1) (cs2) (0) (string_length(cs1) + string_length(cs2)-1) (work)))

let test = string_merge("2222222222222") ("2468");;