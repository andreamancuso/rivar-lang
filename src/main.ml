open Ast
open Codegen

let string_of_token = function
  | Parser.CLASS -> "CLASS"
  | Parser.END -> "END"
  | Parser.FEATURE -> "FEATURE"
  | Parser.COLON -> "COLON"
  | Parser.STRINGLIT s -> "STRINGLIT(" ^ s ^ ")"
  | Parser.TYPE_INTEGER -> "TYPE_INTEGER"
  | Parser.TYPE_BOOLEAN -> "TYPE_BOOLEAN"
  | Parser.TYPE_STRING -> "TYPE_STRING"
  | Parser.REQUIRE -> "REQUIRE"
  | Parser.DO -> "DO"
  | Parser.ENSURE -> "ENSURE"
  | Parser.LPAREN -> "LPAREN"
  | Parser.RPAREN -> "RPAREN"
  | Parser.PLUS -> "PLUS"
  | Parser.MINUS -> "MINUS"
  | Parser.STAR -> "STAR"
  | Parser.SLASH -> "SLASH"
  | Parser.GT -> "GT"
  | Parser.LT -> "LT"
  | Parser.EQ -> "EQ"
  | Parser.OR -> "OR"
  | Parser.AND -> "AND"
  | Parser.NEQ -> "NEQ"
  | Parser.LE -> "LE"
  | Parser.GE -> "GE"
  | Parser.ASSIGN -> "ASSIGN"
  | Parser.OLD -> "OLD"
  | Parser.PRINT -> "PRINT"
  | Parser.EOF -> "EOF"
  | Parser.COMMA -> "COMMA"
  | Parser.ARROW -> "ARROW"
  | Parser.THIS -> "THIS"
  | Parser.RETURN -> "RETURN"
  | Parser.IDENT s -> "IDENT(" ^ s ^ ")"
  | Parser.INT s -> "INT(" ^ s ^ ")"


let rec dump_tokens lexbuf =
  let tok = Lexer.token lexbuf in
  Printf.printf "Token: %s\n%!" (string_of_token tok);
  if tok <> Parser.EOF then dump_tokens lexbuf

let string_of_type = function
  | TypeInteger -> "INTEGER"
  | TypeBoolean -> "BOOLEAN"
  | TypeString -> "STRING"

let print_param { param_name; param_type } =
  Printf.printf "      Param: %s : %s\n" param_name (string_of_type param_type)

let rec string_of_expr = function
  | IntLit i -> string_of_int i
  | BoolLit b -> string_of_bool b
  | StringLit s -> "\"" ^ s ^ "\""
  | Var (Field, name) -> "this->" ^ name
  | Var (Param, name) -> name
  | Var (Local, name) -> name
  | Old name -> "old " ^ name
  | BinOp (op, lhs, rhs) ->
      let op_str = match op with
        | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
        | Eq -> "==" | Neq -> "!=" | Gt -> ">" | Lt -> "<"
        | Ge -> ">=" | Le -> "<=" | And -> "and" | Or -> "or"
      in
      "(" ^ string_of_expr lhs ^ " " ^ op_str ^ " " ^ string_of_expr rhs ^ ")"
  | UnaryOp (op, e) ->
      let op_str = match op with Not -> "not" | Neg -> "-" in
      "(" ^ op_str ^ " " ^ string_of_expr e ^ ")"
  | Call (target, name, args) ->
      let arg_strs = List.map string_of_expr args in
      string_of_expr target ^ "." ^ name ^ "(" ^ String.concat ", " arg_strs ^ ")"

let rec print_stmt indent = function
  | Assign (lhs, rhs) ->
    Printf.printf "%sAssign: %s = %s\n" indent
      (string_of_expr lhs) (string_of_expr rhs)
  | If (cond, then_branch, else_branch) ->
      Printf.printf "%sIf: %s\n" indent (string_of_expr cond);
      List.iter (print_stmt (indent ^ "  ")) then_branch;
      (match else_branch with
        | Some else_stmts ->
            Printf.printf "%sElse:\n" indent;
            List.iter (print_stmt (indent ^ "  ")) else_stmts
        | None -> ())
  | While (cond, body) ->
      Printf.printf "%sWhile: %s\n" indent (string_of_expr cond);
      List.iter (print_stmt (indent ^ "  ")) body
  | Return (Some expr) ->
      Printf.printf "%sReturn: %s\n" indent (string_of_expr expr)
  | Print expr ->
      Printf.printf "%sPrint: %s\n" indent (string_of_expr expr)
  | Return None ->
      Printf.printf "%sReturn\n" indent
    

let print_expr e =
  Printf.printf "        Expr: %s\n" (string_of_expr e)

let () =
  let filename = Sys.argv.(1) in
  let in_chan = open_in filename in
  let lexbuf = Lexing.from_channel in_chan in

  (* dump_tokens lexbuf; *)

  let ast =
    try
      Parser.program Lexer.token lexbuf
    with
    | Parser.Error ->
        let pos = lexbuf.Lexing.lex_curr_p in
        Printf.eprintf "Parse error at line %d, column %d\n"
          pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol);
        exit 1
  in
  close_in in_chan;

  List.iter (fun cls ->
    Printf.printf "Parsed class: %s\n" cls.class_name;
    List.iter (function
      | Field(name, t) ->
          Printf.printf "  Field: %s : %s\n" name (string_of_type t)
      | Routine r ->
          Printf.printf "  Routine: %s\n" r.name;
          List.iter print_param r.params;
          Printf.printf "    Require:\n";
          List.iter print_expr r.require;
          Printf.printf "    Body:\n";
          List.iter (print_stmt "      ") r.body;
          Printf.printf "    Ensure:\n";
          List.iter print_expr r.ensure
    ) cls.features
  ) ast;

  generate_c_file ast "out.c";
  generate_h_file ast "out.h";