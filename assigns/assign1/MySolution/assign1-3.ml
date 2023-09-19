(** length s is the length (number of bytes/characters) of s **)
let string_length = String.length;;

(** get_at s i is the character at index i in s. This is the same as writing s.[i] **)
let string_get_at(cs:string)(i0:int): char = String.get cs i0;;

(*
assign1-3: 10 points
A 3-letter sequence abc is 132-like
if a < c < b holds. For instance, 123 is
not 132-like; but 596 is 132-like.

A string is 132-avoid if there is no subsequence
abc in this string that is 132-like. Note that a
subsequence of a string may contain NON-CONSEQUTIVE
letters of the string.

For instance, 123456789 is 132-avoid;
For instance, 987654321 is 132-avoid;
For instance, 123465789 is not 132-avoid since the
subsequence 165 is 132-like.

Please implement a function string_avoid_132 that
checks if a given string is 132-avoid; the function
returns true if and only if the given string is 132-
avoid.

fun string_avoid_132(cs: string): bool
*)

let rec helper_2(cs:string) (curr:char) (curr_index:int) (index: int) (a_holder: int): bool = 
  if index >= string_length(cs) - 1 then true else
    if index > string_length(cs) then helper_2(cs) (string_get_at(cs) (curr_index)) (curr_index+1) (curr_index+1) (a_holder) else
      if string_get_at(cs) (curr_index) > string_get_at(cs)(curr_index +1) && string_get_at(cs) (curr_index+1) > string_get_at(cs) (a_holder) then false else 
        helper_2(cs) (string_get_at(cs) (curr_index+1)) (curr_index+1) (curr_index+2) (a_holder)

let rec helper_3(cs:string) (curr: char) (curr_index: int) (index: int) (a_holder: int) : bool = 
  if curr_index >= string_length(cs) - 2 && index == string_length(cs) - 1 then true else
  if index > string_length(cs) then helper_3(cs) (string_get_at(cs) (curr_index+1)) (curr_index+1) (curr_index+1) (a_holder) else
    if string_get_at(cs) (curr_index) > string_get_at(cs)(curr_index+1) then helper_2(cs) (string_get_at(cs) (curr_index+1)) (curr_index) (index) (a_holder)
    else helper_3(cs) (string_get_at(cs) (curr_index+1)) (curr_index+1) (curr_index+2) (a_holder)

let rec helper_1(cs:string) (curr: char) (curr_index: int) (index: int): bool = 
  if curr_index >= string_length(cs) - 3 && index == string_length(cs)-1 then true else
  if index > string_length(cs) then helper_1(cs) (string_get_at(cs) (curr_index+1)) (curr_index+1) (curr_index+1) else
    if string_get_at(cs) (curr_index) < string_get_at(cs) (curr_index + 1) then helper_3(cs) (string_get_at(cs) (curr_index+1)) (curr_index) (index) (curr_index)
    else helper_1(cs) (string_get_at(cs) (curr_index+1)) (curr_index+1) (curr_index+2)

let string_avoid_132 (cs: string): bool = 
  helper_1 (cs) (string_get_at(cs) (0)) (0) (1)

let test = string_avoid_132("123465789");;