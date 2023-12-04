#use "./../../../classlib/OCaml/MyOCaml.ml";;

(*

Please implement the interp function following the
specifications described in CS320_Fall_2023_Project-2.pdf

Notes:
1. You are only allowed to use library functions defined in MyOCaml.ml
   or ones you implement yourself.
2. You may NOT use OCaml standard library functions directly.

*)
type const = 
   | Int of int
   | Bool of bool
   | Unit
   | Sym of string
   | Closure of (string*((string*const) list)*coms)
and
com = 
   | Push of const
   | Pop
   | Swap
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
   | IfElse of coms*coms
   | Bind
   | Lookup
   | Fun of coms
   | Call
   | Return
and
coms = com list

let isEmpty (stack: string) : bool=
   if string_length(stack) == 0 then true else false

let ws : unit parser =
   (many whitespace) >| () 

let ws1 : unit parser =
   (many1 whitespace) >| () 

let parse_num : int parser=
(  let* _ = char('-') in
  let* x = natural in let* _ = ws in pure(-1*x)
)
<|>
(
   let* x = natural in let* _ = ws in pure(x)
)

let parse_bool : const parser=
   (let* _ = char('T') in
   let* _ = char('r') in
   let* _ = char('u') in
   let* _ = char('e') in
   let* _ = ws in
   pure( (Bool (true)))
   )
   <|>
   (let* _ = char('F') in
   let* _ = char('a') in
   let* _ = char('l') in
   let* _ = char('s') in
   let* _ = char('e') in
   let* _ = ws in
   pure( (Bool (false)))
   )

let parse_unit : const parser =
   let* _ = char('U') in
   let* _ = char('n') in
   let* _ = char('i') in
   let* _ = char('t') in
   let* _ = ws in
   pure( (Unit))

(*let parse_char() : char parser =
   let list = ['a';'b';'c';'d';'e';'f';'g';'h';'i';'j';'k';'l';'m';'n';'o';'p';'q';'r';'s';'t';'u';'v';'w';'x';'y';'z'] in
   let* charparse = char(list_foldleft(list)(' ')(fun(res)(element) -> element)) in
   pure(charparse)
*)
let parse_char() : char parser =
   (let* chara = char('a') in pure(chara))<|>(let* chara = char('b') in pure(chara))<|>(let* chara = char('c') in pure(chara))<|>
   (let* chara = char('d') in pure(chara))<|>(let* chara = char('e') in pure(chara))<|>(let* chara = char('f') in pure(chara))<|>
   (let* chara = char('g') in pure(chara))<|>(let* chara = char('h') in pure(chara))<|>(let* chara = char('i') in pure(chara))<|>
   (let* chara = char('j') in pure(chara))<|>(let* chara = char('k') in pure(chara))<|>(let* chara = char('l') in pure(chara))<|>
   (let* chara = char('m') in pure(chara))<|>(let* chara = char('n') in pure(chara))<|>(let* chara = char('o') in pure(chara))<|>
   (let* chara = char('p') in pure(chara))<|>(let* chara = char('q') in pure(chara))<|>(let* chara = char('r') in pure(chara))<|>
   (let* chara = char('s') in pure(chara))<|>(let* chara = char('t') in pure(chara))<|>(let* chara = char('u') in pure(chara))<|>
   (let* chara = char('v') in pure(chara))<|>(let* chara = char('w') in pure(chara))<|>(let* chara = char('x') in pure(chara))<|>
   (let* chara = char('y') in pure(chara))<|>(let* chara = char('z') in pure(chara))<|>(let* chara = char('0') in pure(chara))<|>
   (let* chara = char('1') in pure(chara))<|>(let* chara = char('2') in pure(chara))<|>(let* chara = char('3') in pure(chara))<|>
   (let* chara = char('4') in pure(chara))<|>(let* chara = char('5') in pure(chara))<|>(let* chara = char('6') in pure(chara))<|>
   (let* chara = char('7') in pure(chara))<|>(let* chara = char('8') in pure(chara))<|>(let* chara = char('9') in pure(chara))

let parse_sym() : const parser =
   let* char = many1(parse_char()) in 
   pure (Sym (list_foldleft(char)("")(fun(res)(element) -> res^str(element))))

let parse_constant () : const parser =
   ( let* int = parse_num in
   pure (Int (int)) )
   <|>
   ( parse_bool )
   <|> 
   ( parse_unit )
   <|>
   ( parse_sym())

let rec coms_parse() : com list parser = many(com_parse())
and
com_parse() : com parser =
   (
      let* _ = ws in
   let* _ = char('P') in
   let* _ = char('u') in
   let* _ = char('s') in
   let* _ = char('h') in
   let* _ = ws1 in
   let* push = parse_constant() in
   let* _ = char(';') in
   let* _ = ws in
   pure(Push (push))
   )
   <|>
   (
      let* _ = ws in
   let* _ = char('P') in
   let* _ = char('o') in
   let* _ = char('p') in
   let* _ = char(';') in
   let* _ = ws in
   pure(Pop)
   )
   <|>
   (
      let* _ = ws in
   let* _ = char('S') in
   let* _ = char('w') in
   let* _ = char('a') in
   let* _ = char('p') in
   let* _ = char(';') in
   let* _ = ws in
   pure(Swap)
   )
   <|>
   (
      let* _ = ws in
   let* _ = char('T') in
   let* _ = char('r') in
   let* _ = char('a') in
   let* _ = char('c') in
   let* _ = char('e') in
   let* _ = char(';') in
   let* _ = ws in
   pure(Trace)
   )
   <|>
   (
      let* _ = ws in
      let* _ = char('A') in
      let* _ = char('d') in
      let* _ = char('d') in
      let* _ = char(';') in
      let* _ = ws in
      pure(Add)
   )
   <|>
   (
      let* _ = ws in
      let* _ = char('S') in
      let* _ = char('u') in
      let* _ = char('b') in
      let* _ = char(';') in
      let* _ = ws in
      pure(Sub)
   )
   <|>
   (
      let* _ = ws in
      let* _ = char('M') in
      let* _ = char('u') in
      let* _ = char('l') in
      let* _ = char(';') in
      let* _ = ws in
      pure(Mul)
   )
   <|>
   (
      let* _ = ws in
      let* _ = char('D') in
      let* _ = char('i') in
      let* _ = char('v') in
      let* _ = char(';') in
      let* _ = ws in
      pure(Div)
   )
   <|>
   (
      let* _ = ws in
      let* _ = char('A') in
      let* _ = char('n') in
      let* _ = char('d') in
      let* _ = char(';') in
      let* _ = ws in
      pure(And)
   )
   <|>
   (
      let* _ = ws in
      let* _ = char('O') in
      let* _ = char('r') in
      let* _ = char(';') in
      let* _ = ws in
      pure(Or)
   )
   <|>
   (
      let* _ = ws in
      let* _ = char('N') in
      let* _ = char('o') in
      let* _ = char('t') in
      let* _ = char(';') in
      let* _ = ws in
      pure(Not)
   )
   <|>
   (
      let* _ = ws in
      let* _ = char('L') in
      let* _ = char('t') in
      let* _ = char(';') in
      let* _ = ws in
      pure(Lt)
   )
   <|>
   (
      let* _ = ws in
      let* _ = char('G') in
      let* _ = char('t') in
      let* _ = char(';') in
      let* _ = ws in
      pure(Gt)
   )
   <|>
   (
      let* _ = ws in
      let* _ = char('I') in
      let* _ = char('f') in
      let* _ = ws in
      let* ifstatement = coms_parse() in
      let* _ = ws in
      let* _ = char('E') in
      let* _ = char('l') in
      let* _ = char('s') in
      let* _ = char('e') in
      let* _ = ws in
      let* elsestatement = coms_parse() in
      let* _ = ws in
      let* _ = char('E') in
      let* _ = char('n') in
      let* _ = char('d') in
      let* _ = char(';') in
      let* _ = ws in
      pure(IfElse(ifstatement, elsestatement))
   )
   <|>
   (
      let* _ = ws in
      let* _ = char('B') in
      let* _ = char('i') in
      let* _ = char('n') in
      let* _ = char('d') in
      let* _ = char(';') in
      let* _ = ws in
      pure(Bind)
   )
   <|>
   (
      let* _ = ws in
      let* _ = char('L') in
      let* _ = char('o') in
      let* _ = char('o') in
      let* _ = char('k') in
      let* _ = char('u') in
      let* _ = char('p') in
      let* _ = char(';') in
      let* _ = ws in
      pure(Lookup)
   )
   <|>
   (
      let* _ = ws in
      let* _ = char('F') in
      let* _ = char('u') in
      let* _ = char('n') in
      let* _ = ws in
      let* func = coms_parse() in
      let* _ = ws in
      let* _ = char('E') in
      let* _ = char('n') in
      let* _ = char('d') in
      let* _ = char(';') in
      let* _ = ws in
      pure(Fun (func))
   )
   <|>
   (
      let* _ = ws in
      let* _ = char('C') in
      let* _ = char('a') in
      let* _ = char('l') in
      let* _ = char('l') in
      let* _ = char(';') in
      let* _ = ws in
      pure(Call)
   )
   <|>
   (
      let* _ = ws in
      let* _ = char('R') in
      let* _ = char('e') in
      let* _ = char('t') in
      let* _ = char('u') in
      let* _ = char('r') in
      let* _ = char('n') in
      let* _ = char(';') in
      let* _ = ws in
      pure(Return)
   )

let parse(s:string) : com list option =
let cs = string_listize s in
match coms_parse()(cs) with
| Some (e, []) -> Some e
| _ -> None

(*let test = parse (
   "
   Push True;
   If
      Push 14;
      Push 15;
      Add;
      Trace;
   Else
      Push False;
      Trace;
   End;
   Trace;
   ");; 
*)

let is_int(x : const) : bool =
match x with
   | Int (x) -> true
   | _ -> false

let is_bool(x : const) : bool =
match x with
   | Bool (x) -> true
   | _ -> false

let is_unit(x : const) : bool =
match x with 
   | Unit -> true
   | _ -> false
 
let is_sym(x : const) : bool =
match x with
   | Sym (x) -> true
   | _ -> false

let is_closure(x: const) : bool =
match x with 
   | Closure (x) -> true
   | _ -> false

let toString(input : const): string =
match input with
| Bool(false) -> "False"
| Bool(true) -> "True"
| Unit -> "Unit"
| Int(x) -> string_of_int(x)
| Sym(x) -> x

let to_int(input: const) : int =
match input with
| Int(x) -> x

let to_bool(input: const) : bool =
match input with
| Bool(x) -> x

let rec process_enviroment (stackhead) (stackrest) (trace) (enviroment) (comtail) =
   (match enviroment with
   | [] -> "Panic"::trace
   | (symbol, value) :: rest when symbol = toString(stackhead)-> 
         big_interp(value::stackrest)(trace)((symbol, value) :: rest)(comtail)
   | _ :: rest_enviroment ->
      process_enviroment (stackhead) (stackrest) (trace) (rest_enviroment) (comtail))
and
 big_interp (stack : const list) (trace: string list) (enviroment: (string*const) list) (prog: com list) : string list=
   (match prog with
   | [] -> trace
   | comhead :: comtail -> (match comhead with
      | Push (x) -> big_interp(x :: stack)(trace)(enviroment)(comtail)
      | Pop -> (match stack with
               | [] -> "Panic"::trace
               | stackhead :: stacktail -> big_interp(stacktail)(trace)(enviroment)(comtail)
      )
      | Swap -> (match stack with
               | [] -> "Panic"::trace
               | stackhead :: [] -> "Panic"::trace
               | stackhead1 :: stackhead2 :: stacktail -> big_interp(stackhead2::stackhead1::stacktail)(trace)(enviroment)(comtail)
      )
      | Trace -> (match stack with
               | [] -> "Panic"::trace
               | stacktracehead :: stacktracetail -> big_interp(Unit::stacktracetail)(toString(stacktracehead)::trace)(enviroment)(comtail)
      )
      | Add -> (match stack with
               | [] -> "Panic"::trace
               | x :: [] -> "Panic"::trace
               | x :: y :: _ when not (is_int x) || not (is_int y) -> "Panic"::trace
               | x :: y :: stackrest -> let result = to_int(x)+to_int(y) in big_interp(Int(result)::stackrest)(trace)(enviroment)(comtail)
      )
      | Sub -> (match stack with
               | [] -> "Panic"::trace
               | x :: [] -> "Panic"::trace
               | x :: y :: _ when not (is_int x) || not (is_int y) -> "Panic"::trace
               | x :: y :: stackrest -> let result = to_int(x)-to_int(y) in big_interp(Int(result)::stackrest)(trace)(enviroment)(comtail)
      )
      | Mul -> (match stack with
               | [] -> "Panic"::trace
               | x :: [] -> "Panic"::trace
               | x :: y :: _ when not (is_int x) || not (is_int y) -> "Panic"::trace
               | x :: y :: stackrest -> let result = to_int(x)*to_int(y) in big_interp(Int(result)::stackrest)(trace)(enviroment)(comtail)
      )
      | Div -> (match stack with
               | [] -> "Panic"::trace
               | x :: [] -> "Panic"::trace
               | x :: y :: _ when not (is_int x) || not (is_int y) || to_int y == 0 -> "Panic"::trace
               | x :: y :: stackrest -> let result = to_int(x)/to_int(y) in big_interp(Int(result)::stackrest)(trace)(enviroment)(comtail)
      )
      | And -> (match stack with
               | [] -> "Panic"::trace
               | x :: [] -> "Panic"::trace
               | x :: y :: _ when not (is_bool x) || not (is_bool y) -> "Panic"::trace
               | x :: y :: stackrest -> let result = to_bool(x) && to_bool(y) in big_interp(Bool(result)::stackrest)(trace)(enviroment)(comtail)
      )
      | Or -> (match stack with
               | [] -> "Panic"::trace
               | x :: [] -> "Panic"::trace
               | x :: y :: _ when not (is_bool x) || not (is_bool y) -> "Panic"::trace
               | x :: y :: stackrest -> let result = to_bool(x) || to_bool(y) in big_interp(Bool(result)::stackrest)(trace)(enviroment)(comtail)
      )
      | Not -> (match stack with
               | [] -> "Panic"::trace
               | x :: _ when not (is_bool x) -> "Panic"::trace
               | x :: stackrest -> let result = not(to_bool(x)) in big_interp(Bool(result)::stackrest)(trace)(enviroment)(comtail)
      )
      | Lt -> (match stack with
               | [] -> "Panic"::trace
               | x :: [] -> "Panic"::trace
               | x :: y :: _ when not (is_int x) || not (is_int y) -> "Panic"::trace
               | x :: y :: stackrest -> let result = (to_int(x)<to_int(y)) in big_interp(Bool(result)::stackrest)(trace)(enviroment)(comtail)
      )
      | Gt -> (match stack with
               | [] -> "Panic"::trace
               | x :: [] -> "Panic"::trace
               | x :: y :: _ when not (is_int x) || not (is_int y) -> "Panic"::trace
               | x :: y :: stackrest -> let result = (to_int(x)>to_int(y)) in big_interp(Bool(result)::stackrest)(trace)(enviroment)(comtail)
      )
      | IfElse(x, y) -> (match stack with
               | [] -> "Panic"::trace
               | stackhead :: _ when not (is_bool stackhead) -> "Panic"::trace
               | stackhead :: stackrest -> (match stackhead with
                                    | Bool(true) -> big_interp(stackrest)(trace)(enviroment)(list_append(x)(comtail))
                                    | Bool(false) -> big_interp(stackrest)(trace)(enviroment)(list_append(y)(comtail))
      )
      )
      | Bind -> (match stack with
               | [] -> "Panic"::trace
               | stackhead :: [] -> "Panic"::trace
               | stackhead :: _ when not (is_sym stackhead) -> "Panic"::trace
               | stackhead :: value :: stackrest (*when (is_int(value)) || (is_bool(value)) || (is_unit(value)) || (is_sym(value)) *) -> 
                  big_interp(stackrest)(trace)(((toString stackhead), value) :: enviroment)(comtail)
      )
      | Lookup -> (match stack with 
               | [] -> "Panic"::trace
               | stackhead :: _ when not (is_sym stackhead) -> "Panic"::trace
               | stackhead :: stackrest -> process_enviroment (stackhead) (stackrest) (trace) (enviroment) (comtail)
                                          
      )
      | Fun (x)-> (match stack with
               | [] -> "Panic"::trace
               | stackhead :: _ when not (is_sym stackhead) -> "Panic"::trace
               | stackhead :: stackrest -> big_interp(Closure (toString(stackhead), enviroment, x)::stackrest)(trace)(enviroment)(comtail)
      )
      | Call -> (match stack with 
               | [] -> "Panic"::trace
               | x :: [] -> "Panic"::trace
               | closure :: a :: _ when not(is_closure closure) -> "Panic"::trace
               | Closure(f, vf, c) :: a :: stackrest -> 
                     big_interp(a::Closure("cc", enviroment, comtail)::stackrest)(trace)((f, Closure(f, vf, c))::vf)(c)
      )
      | Return -> (match stack with 
               | [] -> "Panic"::trace
               | x :: [] -> "Panic"::trace
               | closure :: a :: _ when not(is_closure closure) -> "Panic"::trace
               | Closure(f, vf, c) :: a :: stackrest -> 
                     big_interp(a::stackrest)(trace)(vf)(c)
      )
      
   )) 

let interp (s : string) : string list option = (* YOUR CODE *)
   let parsed = parse(s) in
   match parsed with
   | None -> None
   | Some (x) -> Some (big_interp([])([])([])(x))

   let test = interp ("
   Push factorial;
   ");; 
   