#use "./../../../../classlib/OCaml/MyOCaml.ml";;

let list_append(xs : 'a list) (ys : 'a list):'a list =
  list_foldright(xs) (ys) (fun(element)(res)   -> element :: res)

let k_combo = fun x y -> x
let s_combo = fun x y z -> x z (y z)
let church_two = fun f x -> f( f( x ) )
let church_three = fun f x -> f( f( f( x ) ) )

let mystery1 =
  s_combo(k_combo)(k_combo)(fun x -> 10)(100)

let mystery2 = church_three(fun x -> x+1)(0)

let mystery3 =
  church_three(church_two)(fun x -> x+1)(0)
;;