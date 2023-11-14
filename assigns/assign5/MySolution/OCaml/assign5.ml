#use "./../../../../classlib/OCaml/MyOCaml.ml";;

(*let string_listsize (s : string) : char list =
  list_make_fwork(fun work -> string_foreach s work)

let string_of_list (list: char list) : string =
  string_make_fwork(fun work -> list_foreach s work)

(* char list is the rest of the expression*)
let parse_digit (cs: char list) : (int * char list) option =
  match cs with
  | [] -> None
  | c :: cs0 ->
    let i = ord c - ord '0' in
    if 0 <= i && i <= 9
      then Some (i, cs0)
      else None

let int_of_list(xs: int list) : int =
  let rec loop xs acc = 
    match xs with 
    | [] -> acc
    | x :: xs -> loop xs (acc * 10 + x)
  in loop xs 0

(* num is recursive in grammer so that means function is recursive*)
let rec parse_num (cs : char list) : (int * char list) option =
  let rec parse_aux cs : (int list * char list) option =
  (* checking if it is just one digit*)
    match parse_digit cs with
    | None -> None
    | Some (i, cs0) ->
      match parse_num cs0 with
      | Some(i', cs1) -> Some (x :: xs, cs1)
      | None -> Some ([x], cs0)
    in
  match parse_aux with
  | Some(xs, cs0) -> Some(int_of_list xs, trim cs0)
  | None -> None

let parse_word (s : string) (cs: char list) : (unit * char list) option =
  let cs0 = string_listsize s int_of_list
  let rec loop cs cs0 =
    match cs, cs0 with
    | _, [] -> Some ((), trim cs)
    | [], _ -> None
    | c :: cs, c0 :: cs0 ->
      if c = c0
        the loop cs cs0 else None
    in
    loop cs cs0

type 'a parser = char list -> ('a * char list) option

(* takes in a list of parsers at the same time*)
let rec choice (ps : char list -> ('a * char list) option) list (c : char list): ('a * char list) option =
  match ps with
  | [] -> None
  | p :: ps ->
    match p cs with
    | Some (x, cs0) -> Some (x, cs0)
    | None -> choice ps0 cs

let rec parse_expr (xs: char list) : (expr * char list) option =
  choice [parse_int, parse_add, parse_mul]

and let parse_exprs (cs : char list) : (expr list * char list) option =
  let rec parse_aux cs : (int list * char list) option =
    (* checking if it is just one digit*)
      match parse_digit cs with
      | None -> None
      | Some (x, cs0) ->
        match parse_num cs0 with
        | Some(xs, cs1) -> Some (x :: xs, cs1)
        | None -> Some ([x], cs0)
      in
    parse_aux cs

and parse_int (cs : char list) : (expr * char list) option =
  match parse_num cs with
  | Some (x, cs0) -> Some (Int x, cs0)
  | None -> None

and parse_add (xs : char list) : (expr * char list) option =
  match parse_word "(add" cs with
  |None -> None
  | Some (_, cs0) ->
    Match parse_expr cs0 with
    | None -> None
    | Some (es, cs1) -> 
      match parse_word ")" cs1 with
      | None -> None
      | Some (_, cs2) -> Some (Add es, cs2)

and parse_mul (xs : char list) : (expr * char list) option =
  match parse_word "(add" cs with
  | None -> None
  | Some (_, cs0) ->
    match parse_expr cs0 with
    | None -> None
    | Some (es, cs1) -> 
      match parse_word ")" cs1 with
      | None -> None
      | Some (_, cs2) -> Some (Mul es, cs2)

let parse (s : string) : expr option =
  let cs = string_listsize s int_of_list
  match parse _expr cs with
  | Some (e, []) -> Some e
  | _ -> None
*)

string_parse (bind read char) "LLswbeofaiwne";;

string_parse (seqright read (char 'a')) "hallo";;

