#use "./../../../classlib/OCaml/MyOCaml.ml";;
(*
assign1-5: 20 points

A sequence of chars is ascending if any char in
the sequence is less than or equal to the following
one (when the following one does exist).
Given a string cs, please implement a function
that find the longest ascending subsequence of [cs].
If there are more than one such sequences, the left
most one should be returned.

fun string_longest_ascend(xs: string): string

For instance, given "1324561111", the function
string_longest_ascend returns "13456"

For instance, given "1234561111", the function
string_longest_ascend returns "123456"

For instance, given "1234511111", the function
string_longest_ascend returns "111111".
*)  
let rec substring (xs: string) (trav_index) (start: int) (finish: int): string = 
  if trav_index > finish then "" else str(string_get_at(xs) (trav_index)) ^ substring (xs:string) (trav_index+1) (start) (finish)

let rec helper (xs: string) (index: int) (trav_index: int) (max: int) (start: int) (finish: int): string = 
  if index >= string_length(xs) - 1 then substring (xs) (index) (index) (finish) else
    if ord(string_get_at(xs) (index)) <= ord(string_get_at(xs) (index+1)) then helper(xs) (index) (index) (trav_index) (index) (index+1) else
      if finish - start + 1 > max then helper (xs) (index + 1) (index + 2) (finish-start+1) (index+1) (index+2) else
      helper(xs) (index+1) (index+2) (max) (index+1) (index+2)
 
let string_longest_ascend(xs: string): string = 
  (print_endline) ("Is this even working");
  helper (xs) (0) (1) (0) (0) (0)
let test = string_longest_ascend("1324561111");;