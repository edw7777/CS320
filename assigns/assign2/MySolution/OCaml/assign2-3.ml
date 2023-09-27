(*
//
Assign2-3: 10 points
//
Please implement foldleft_to_iforeach that turns a
foldleft-loop into a iforeach-loop:
let
foldleft_to_iforeach
(foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach = ...
*)
type ('xs, 'x0) iforeach = 'xs -> (int -> 'x0 -> unit) -> unit

type ('xs, 'x0, 'r0) foldleft = 'xs -> 'r0 -> ('r0 -> 'x0 -> 'r0) -> 'r0

let rec
list_foldleft
(xs: 'a list)
(r0: 'r0)(fopr: 'r0 -> 'a -> 'r0): 'r0 =
match xs with
| [] -> r0
| (x1 :: xs) ->
  list_foldleft(xs)(fopr(r0)(x1))(fopr)
;;
  
let foldleft_to_iforeach (foldleft: ('xs, 'x0, int) foldleft): ('xs, 'x0) iforeach =
  fun xs work ->
    let rec iterate index acc = function
      | [] -> ()
      | x :: rest ->
        work index x;
        iterate (index + 1) (foldleft xs acc (fun acc x -> acc)) rest
    in
    iterate 0 0 xs