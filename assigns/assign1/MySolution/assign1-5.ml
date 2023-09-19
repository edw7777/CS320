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
  if trav_index > finish then "" else string_get_at(xs) (trav_index) ^ substring (xs:string) (trav_index+1) (start) (finish)

let rec helper (xs: string) (index: int) (trav_index: int) (start: int) (finish: int): string = 
  if index >= string_length(xs) - 1 then substring (xs) (index) (index) (finish) else
    if string_get_at(xs) (index) <= string_get_at(xs) (index+1) then helper(xs) (index) (index) (trav_index) (index) (index+1) else
      helper(xs)
 
let string_longest_ascend(xs: string): string = 
  helper (xs) (0) (1) (0) (0)