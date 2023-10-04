#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(*
Assign3-4:
HX-2023-09-26: 20 points
Given a word x of length n, another word is a buddy
of x if x and y differ exactly at one position. For
instance, "live" is a buddy "love" (and "love" is also
a buddy of "live").
//
Please give a NON-RECURSIVE implementation of
list_of_buddies that returns a list of all the buddies
of a given word.
//
let
list_of_buddies(word: string): string list = ...

(*
FYI. The concept of word buddies is used in the following game:
https://xanadu-lang.github.io/xats2js/docgen/CodeBook/Doublet/2020-11-29/
https://github.com/xanadu-lang/xats2js/tree/master/docgen/CodeBook/Doublet
*)
*)


let helper(word: string) (index: int) =
  (*let (a, 0)*) 
  (*string_foldleft (word) ([], 0) (fun(res)(index) -> string_foldleft ("abcdefghijklmnopqrstuvwxyz") ("") (fun(res)(char) -> x1) ^ word )*)
  string_foldleft ("abcdefghijklmnopqrstuvwxyz") ([]) ( 
    fun(acc) (letter) -> let (output, _) = 
    string_foldleft(word) ("", 0) (fun(res, iter_index)(element) -> if index == iter_index then ((res ^ str(letter)), iter_index+1) else (res ^ str(element), iter_index+1)) in output :: acc)

(* the work is same type as accumulator, the char, puts it into the accumulatr*)
  (*string_foldleft takes in string, function 'a->'b->'c accumulator maybe? and then the work function*)

let list_of_buddies(word:string): string list =
  let (lebron, _) = string_foldleft (word) ([], 0) (fun(res, index)(_) -> ((list_append (res) (helper(word) (index))), index+1)) in 
  let (output, _) = list_foldleft (lebron) ([], 0) (fun(acc, iter_index)(variation) -> if word = variation then (acc, iter_index+1) else ((variation :: acc), iter_index+1))in output

let test = list_of_buddies("love");;