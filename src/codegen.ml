open Ast
open Printf

let rec string_of_expr = function
  | IntLit i -> string_of_int i
  | StringLit s -> sprintf "\"%s\"" s
  | Var (Field, name) -> "self->" ^ name
  | Var (Param, name) -> name
  | Var (Local, name) -> name
  | Old name -> "old_" ^ name
  | BinOp (Eq, a, b) ->
      (match a, b with
       | StringLit _, _
       | _, StringLit _
       | Var (_, _), Var (_, _) -> sprintf "strcmp(%s, %s) == 0" (string_of_expr a) (string_of_expr b)
       | _ -> sprintf "(%s == %s)" (string_of_expr a) (string_of_expr b))
  | BinOp (Neq, a, b) ->
      (match a, b with
       | StringLit _, _
       | _, StringLit _
       | Var (_, _), Var (_, _) -> sprintf "strcmp(%s, %s) != 0" (string_of_expr a) (string_of_expr b)
       | _ -> sprintf "(%s != %s)" (string_of_expr a) (string_of_expr b))
  | BinOp (op, a, b) ->
      let op_str = match op with
        | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
        | Gt -> ">" | Lt -> "<" | Ge -> ">=" | Le -> "<=" | And -> "&&" | Or -> "||"
        | _ -> "/* unknown op */"
      in
      "(" ^ string_of_expr a ^ " " ^ op_str ^ " " ^ string_of_expr b ^ ")"
  | _ -> "/* unsupported expr */"

let string_of_type = function
  | TypeInteger -> "int32_t"
  | TypeBoolean -> "bool"
  | TypeString -> "const char*"

let gen_stmt = function
  | Assign (Var(_, name), expr) ->
      (match expr with
       | StringLit _ -> sprintf "    self->%s = strdup(%s);\n" name (string_of_expr expr)
       | _ -> sprintf "    self->%s = %s;\n" name (string_of_expr expr))
  | Assign (lhs, rhs) ->
      sprintf "    %s = %s;\n" (string_of_expr lhs) (string_of_expr rhs)
  | Print expr ->
      sprintf "    printf(\"%%s\\n\", %s);\n" (string_of_expr expr)
  | Return (Some expr) ->
      sprintf "    return %s;\n" (string_of_expr expr)
  | Return None ->
      "    return;\n"
  | _ -> "    /* unsupported stmt */\n"

let gen_routine class_name r cls_fields =
  let param_list =
    String.concat ", " (List.map (fun p ->
      sprintf "%s %s" (string_of_type p.param_type) p.param_name
    ) r.params)
  in
  let buffer = Buffer.create 256 in

  let signature =
    if param_list = "" then
      sprintf "void %s(%s* self)" r.name class_name
    else
      sprintf "void %s(%s* self, %s)" r.name class_name param_list
  in
  Buffer.add_string buffer (signature ^ " {\n");

  List.iter (fun e ->
    Buffer.add_string buffer (sprintf "    if (!(%s)) {\n" (string_of_expr e));
    Buffer.add_string buffer "        fprintf(stderr, \"Precondition failed\\n\");\n";
    Buffer.add_string buffer "        exit(1);\n    }\n"
  ) r.require;

  let used_old_vars =
    let rec collect acc = function
      | Old name -> name :: acc
      | BinOp (_, a, b) -> collect (collect acc a) b
      | UnaryOp (_, e) -> collect acc e
      | Call (_, _, args) -> List.fold_left collect acc args
      | _ -> acc
    in
    List.fold_left (fun acc e -> collect acc e) [] r.ensure
    |> List.sort_uniq String.compare
  in
  List.iter (fun name ->
    let field_type =
      match List.find_opt (function Field(n, _) -> n = name | _ -> false) cls_fields with
      | Some (Field(_, t)) -> string_of_type t
      | _ -> "/* unknown type */"
    in
    Buffer.add_string buffer (sprintf "    %s old_%s = self->%s;\n" field_type name name)
  ) used_old_vars;

  let has_result =
    match r.return_type with
    | Some _ -> true
    | None -> false
  in

  if has_result then
    Buffer.add_string buffer (sprintf "    %s _result;\n" (string_of_type (Option.get r.return_type)));
  
  List.iter (fun stmt ->
    match stmt with
    | Return (Some expr) when has_result ->
        Buffer.add_string buffer (sprintf "    _result = %s;\n    return _result;\n" (string_of_expr expr))
    | Return None ->
        Buffer.add_string buffer "    return;\n"
    | _ ->
        Buffer.add_string buffer (gen_stmt stmt)
  ) r.body;

  List.iter (fun e ->
    let skip =
      match e with
      | BinOp (_, Var (_, name), _) when name = "result" && not has_result -> true
      | BinOp (_, _, Var (_, name)) when name = "result" && not has_result -> true
      | _ -> false
    in
    if not skip then
      let expr_str =
        match e with
        | BinOp (op, Var (_, "result"), rhs) ->
            string_of_expr (BinOp (op, Var (Local, "_result"), rhs))
        | BinOp (op, lhs, Var (_, "result")) ->
            string_of_expr (BinOp (op, lhs, Var (Local, "_result")))
        | _ -> string_of_expr e
      in
      Buffer.add_string buffer (sprintf "    if (!(%s)) {\n" expr_str);
      Buffer.add_string buffer "        fprintf(stderr, \"Postcondition failed\\n\");\n";
      Buffer.add_string buffer "        exit(1);\n    }\n"
  ) r.ensure;

  Buffer.add_string buffer "}\n";

  Buffer.contents buffer

let gen_header cls =
  let class_name = String.uppercase_ascii cls.class_name in
  let buf = Buffer.create 256 in
  Buffer.add_string buf ("#ifndef RIVAR_H\n#define RIVAR_H\n\n");
  Buffer.add_string buf ("#include <stdint.h>\n#include <stdbool.h>\n\n");
  Buffer.add_string buf (sprintf "typedef struct {\n");
  List.iter (function
    | Field(name, t) ->
        Buffer.add_string buf (sprintf "    %s %s;\n" (string_of_type t) name)
    | _ -> ()
  ) cls.features;
  Buffer.add_string buf (sprintf "} %s;\n\n" class_name);
  List.iter (function
    | Routine r ->
        let params =
          String.concat ", " (List.map (fun p ->
            sprintf "%s %s" (string_of_type p.param_type) p.param_name
          ) r.params)
        in
        let signature =
          if params = "" then
            sprintf "void %s(%s* self);" r.name class_name
          else
            sprintf "void %s(%s* self, %s);" r.name class_name params
        in
        Buffer.add_string buf (signature ^ "\n")
    | _ -> ()
  ) cls.features;
  Buffer.add_string buf ("\n#endif // RIVAR_H\n");
  Buffer.contents buf

let gen_class cls =
  let class_name = String.uppercase_ascii cls.class_name in
  let buf = Buffer.create 256 in

  Buffer.add_string buf "#include <stdio.h>\n#include <stdlib.h>\n#include <stdbool.h>\n#include <stdint.h>\n#include <string.h>\n\n";
  Buffer.add_string buf (sprintf "typedef struct {\n");
  List.iter (function
    | Field(name, t) ->
        Buffer.add_string buf (sprintf "    %s %s;\n" (string_of_type t) name)
    | _ -> ()
  ) cls.features;
  Buffer.add_string buf (sprintf "} %s;\n\n" class_name);

  List.iter (function
    | Routine r -> Buffer.add_string buf (gen_routine class_name r cls.features)
    | _ -> ()
  ) cls.features;

  Buffer.contents buf

let generate_c_file (prog : program) (filename : string) =
  let oc = open_out filename in
  List.iter (fun cls ->
    let code = gen_class cls in
    output_string oc code
  ) prog;
  close_out oc

let generate_h_file (prog : program) (filename : string) =
  let oc = open_out filename in
  List.iter (fun cls ->
    let hdr = gen_header cls in
    output_string oc hdr
  ) prog;
  close_out oc
