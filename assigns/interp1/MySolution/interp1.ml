#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-1.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)

type const = 
   | Int of int
   | Bool of bool
   | Unit

type com = 
   | Push of const
   | Pop
   | Trace
   | Add
   | Sub
   | Mul
   | Div
   | And
   | Or
   | Not
   | Lt
   | Gt

type coms = com list

type prog = coms

let rec prog_parse = 
   

let interp (s : string) : string list option = (* YOUR CODE *)
   let cs =  string_listsize s in
   match prog_parse()(cs) with
   | Some (e, []) -> Some e
   | _ -> None