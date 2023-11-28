#use "./../../../../classlib/OCaml/MyOCaml.ml";;

type sexpr =
  | SInt of int        (* 1, 2, 3, 4 ...  *)
  | SAdd of sexpr list (* (add e1 e2 ...) *)
  | SMul of sexpr list (* (mul e1 e2 ...) *) 
let ws : unit parser =
  (many whitespace) >| () 
let parse_num : int parser= 
  let* x = natural in let* _ = ws in pure(x)

let rec exprs_parse() = many1(expr_parse())
and
expr_parse() : sexpr parser= 
  (
  let* x = parse_num in 
  pure(SInt (x))
  )
  <|>
  (let* _ = char('(') in
  let* _ = char('a') in
  let* _ = char('d') in
  let* _ = char('d') in
  let* _ = char(' ') in
  let* insideexpr = exprs_parse() in
  let* _ = char(')') in
  let* _ = ws in  (* Need to remove spaces*)
  (* Some (SAdd (insideexpr), ")") *)
  pure(SAdd (insideexpr))
  )
  <|>
  (
  let* _ = char('(') in
  let* _ = char('m') in
  let* _ = char('u') in
  let* _ = char('l') in
  let* _ = char(' ') in
  let* insideexpr = exprs_parse() in
  let* _ = char(')') in
  let* _ = ws in  (* Need to remove spaces*)
  pure(SMul (insideexpr))
  ) 

let sexpr_parse(s : string) : sexpr option = 
  let cs = string_listize s in
  match expr_parse()(cs) with
  | Some (e, []) -> Some e
  | _ -> None

(*
let rec sexpr_to_string (e: sexpr) : string = 
  match e with
  | SInt(x) -> string_of_int(x)
  | SAdd(x) -> "(add" ^ list_foldleft(x)("")(fun (acc)(expr) -> acc ^ " " ^ sexpr_to_string(expr)) ^ ")"
  | SMul(x) -> "(mul" ^ list_foldleft(x)("")(fun (acc)(expr) -> acc ^ " " ^ sexpr_to_string(expr)) ^ ")"

let test1 = sexpr_to_string((SMul [SAdd [SInt 1; SInt 2]; SInt 3; SMul [SInt 1]]))
*)
let rec sexpr_to_string (e: sexpr) : string = 
  match e with
  | SInt(x) -> str(char_of_digit(x))
  | SAdd(x) -> string_append(string_append("(add") (list_foldleft(x)("")(fun (acc)(expr) -> 
    let y = string_append (acc) (" ") in 
    string_append(y)(sexpr_to_string(expr))
  ))) (")")
  | SMul(x) -> string_append(string_append("(mul") (list_foldleft(x)("")(fun (acc)(expr) -> 
    let y = string_append (acc) (" ") in 
    string_append(y)(sexpr_to_string(expr))
  ))) (")")
