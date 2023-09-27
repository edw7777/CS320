type 'a mylist =
  | MyNil
  | MyCons of 'a * 'a mylist
  | MySnoc of 'a mylist * 'a
  | MyReverse of 'a mylist
  | MyAppend2 of 'a mylist * 'a mylist

(* ****** ****** *)
exception MySubscript;;
(* ****** ****** *)

let rec
mylist_foreach
(xs: 'a mylist)
(work: 'a -> unit): unit =
match xs with
| MyNil -> ()
| MyCons(x1, xs) ->
  (work(x1); mylist_foreach(xs)(work))
| MySnoc(xs, x1) ->
  (mylist_foreach(xs)(work); work(x1))
| MyReverse(xs) -> mylist_rforeach(xs)(work)
| MyAppend2(xs1, xs2) ->
  (mylist_foreach(xs1)(work); mylist_foreach(xs2)(work))

and
mylist_rforeach
(xs: 'a mylist)
(work: 'a -> unit): unit =
match xs with
| MyNil -> ()
| MyCons(x1, xs) ->
  (mylist_rforeach(xs)(work); work(x1))
| MySnoc(xs, x1) ->
  (work(x1); mylist_rforeach(xs)(work))
| MyReverse(xs) -> mylist_foreach(xs)(work)
| MyAppend2(xs1, xs2) ->
  (mylist_rforeach(xs2)(work); mylist_rforeach(xs1)(work))
;;

(* ****** ****** *)
let
mylist_subscript_exn
( (*void*) ): 'a = raise MySubscript;;
(* ****** ****** *)

let rec mylist_length(xs: 'a mylist): int = 
  match xs with
| MyNil -> 0
| MyCons (_, tail) -> 1 + mylist_length(tail)
| MySnoc (init, _) -> mylist_length(init) +1
| MyReverse l -> mylist_length(l)
| MyAppend2 (l1, l2) -> mylist_length(l1) + mylist_length(l2)


(*
//
Assign2-2: 10 points
//
Please implement mylist_get_at based
on pattern matching that computes the
element of a given mylist at a given
position.
//
You should call mylist_subscript_exn if
the give position is out-of-bounds.
//
let rec
mylist_get_at(xs: 'a mylist)(i0: int): 'a = ...
//
*)

let rec mylist_get_at (xs: 'a mylist) (i0: int): 'a =
    match xs with
    | MyNil -> mylist_subscript_exn()
    | MyCons (x, _) when i0 = 0 -> x 
    | MyCons (_, rest) when i0 > 0 -> mylist_get_at rest (i0 - 1) 
    | MySnoc (_, x) when i0 = 0 -> x 
    | MySnoc (init, _) when i0 > 0 -> mylist_get_at init (i0 - 1) 
    | MyReverse l -> mylist_get_at l i0 
    | MyAppend2 (xs1, xs2) ->
        let len1 = mylist_length xs1 in
        if i0 < len1 then
          mylist_get_at xs1 i0 
        else
          mylist_get_at xs2 (i0 - len1) 
    | _ -> mylist_subscript_exn() 
(* ****** ****** *)

type
('xs, 'x0) foreach = 'xs -> ('x0 -> unit) -> unit

type
('xs, 'x0) rforeach = 'xs -> ('x0 -> unit) -> unit

type
('xs, 'x0) iforeach = 'xs -> (int -> 'x0 -> unit) -> unit

type
('xs, 'x0, 'r0) foldleft = 'xs -> 'r0 -> ('r0 -> 'x0 -> 'r0) -> 'r0

(* ****** ****** *)