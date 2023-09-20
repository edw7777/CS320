#use "./../../../classlib/OCaml/MyOCaml.ml";;
(* assign1-6: 20 bonus points
A 4-letter sequence abcd is 1324-like
if a < c < b < d holds. For instance, 1234 is
not 132-like; but 2547 is 1324-like.

A string is 1324-avoid if there is no subsequence
abc in this string that is 1324-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 1324-avoid;
For instance, 987654321 is 1324-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 1657 is 1324-like.

Please implement a function string_avoid_1324 that
checks if a given string is 1324-avoid; the function
returns true if and only if the given string is 1324-
avoid.

fun string_avoid_1324(cs: string): bool
*)
let rec helper_4(cs:string) (curr:char) (curr_index:int) (index: int) (b_holder: int): bool =
  if index >= string_length(cs) - 1 then true else
    if index > string_length(cs) then helper_4(cs) (string_get_at(cs) (curr_index)) (curr_index+1) (curr_index+1) (b_holder) else
      if string_get_at(cs) (curr_index) > string_get_at(cs)(curr_index +1) && string_get_at(cs) (curr_index+1) > string_get_at(cs) (b_holder) then false else 
        helper_4(cs) (string_get_at(cs) (curr_index+1)) (curr_index+1) (curr_index+2) (b_holder)

let rec helper_2(cs:string) (curr:char) (curr_index:int) (index: int) (a_holder: int): bool = 
  if curr_index >= string_length(cs) - 2 && index == string_length(cs)-1 then true else
    if index > string_length(cs) then helper_2(cs) (string_get_at(cs) (curr_index)) (curr_index+1) (curr_index+1) (a_holder) else
      if string_get_at(cs) (curr_index) > string_get_at(cs)(curr_index +1) then helper_4(cs) (string_get_at(cs) (curr_index+1)) (curr_index) (index) (curr_index) else 
        helper_2(cs) (string_get_at(cs) (curr_index+1)) (curr_index+1) (curr_index+2) (a_holder)

let rec helper_3(cs:string) (curr: char) (curr_index: int) (index: int) (a_holder: int) : bool = 
  if curr_index >= string_length(cs) - 3 && index == string_length(cs)-1 then true else
  if index > string_length(cs) then helper_3(cs) (string_get_at(cs) (curr_index+1)) (curr_index+1) (curr_index+1) (a_holder) else
    if string_get_at(cs) (curr_index) > string_get_at(cs)(curr_index+1) then helper_2(cs) (string_get_at(cs) (curr_index+1)) (curr_index) (index) (a_holder)
    else helper_3(cs) (string_get_at(cs) (curr_index+1)) (curr_index+1) (curr_index+2) (a_holder)

let rec helper_1(cs:string) (curr: char) (curr_index: int) (index: int): bool = 
  if curr_index >= string_length(cs) - 4 && index == string_length(cs)-1 then true else
  if index > string_length(cs) then helper_1(cs) (string_get_at(cs) (curr_index+1)) (curr_index+1) (curr_index+1) else
    if string_get_at(cs) (curr_index) < string_get_at(cs) (curr_index + 1) then helper_3(cs) (string_get_at(cs) (curr_index+1)) (curr_index) (index) (curr_index)
    else helper_1(cs) (string_get_at(cs) (curr_index+1)) (curr_index+1) (curr_index+2)

let string_avoid_1324 (cs: string): bool = 
  if string_length(cs) == 0 then true else helper_1 (cs) (string_get_at(cs) (0)) (0) (1)

let test = string_avoid_1324("1324")

(* ****** ****** *)
