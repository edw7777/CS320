#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(*
Assign3-1:
HX-2023-09-26: 10 points
A matrix can be represented as a list of lists.
For instance, [[1;2;3];[4;5;6];[7;8;9]] represents
the following 3x3 square matrix:
1 2 3
4 5 6
7 8 9
Please implement matrix_transpose that returns
the transpose of a given matrix:
let rec
matrix_transpose(xss: 'a list list): 'a list list
For instance, the transpose of the above matrix
is given as follows:
1 4 7
2 5 8
3 6 9
You are allowed to define recursive functions to
solve this problem.
*)
(*let rec get_element_at_index lst index =
  match lst with
  | [] -> []
  | hd :: tl ->
    if index = 0 then [hd]
    else get_element_at_index tl (index - 1)
;;

let list_size(xs: 'a list): int =
  list_foldleft(xs) (0) (fun(res)(x1) -> 1 + res)

let helper (xss: 'a list list): 'a list list = 
  list_foldleft (xss) ([[]], 0) (fun(res, iter_index)(col_index) -> res @ list_foldleft)

let matrix_transpose (xss: 'a list list): 'a list list =
  helper(xss)
;; *)

let test = matrix_transpose([[1;2;3];[4;5;6];[7;8;9]]);;

let rec
matrix_transpose(xss: 'a list list): 'a list list = 
  match xss with
  | [] -> []
  | let res = res @ matrix_transpose(xss)()