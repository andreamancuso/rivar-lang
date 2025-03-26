open Ast
open Printf

let rec string_of_expr = function
  | IntLit i -> string_of_int i
  | StringLit s -> sprintf "\"%s\"" s
  | Var (Field, name) -> "self->" ^ name
  | Var (Param, name) -> name
  | Var (Local, name) -> name
  | Old name -> "old_" ^ name
  | BinOp (op, a, b) ->
    let op_str = match op with
      | Add -> "+" | Sub -> "-" | Mul -> "*" | Div -> "/"
      | Eq -> "==" | Neq -> "!=" | Gt -> ">" | Lt -> "<"
      | Ge -> ">=" | Le -> "<=" | And -> "&&" | Or -> "||"
    in
    (match (a, b) with
     | (Var (Field, x), Old y) when x = y && op = Eq ->
         sprintf "strcmp(self->%s, old_%s) == 0" x y
     | (Old x, Var (Field, y)) when x = y && op = Eq ->
         sprintf "strcmp(old_%s, self->%s) == 0" x y
     | _ -> "(" ^ string_of_expr a ^ " " ^ op_str ^ " " ^ string_of_expr b ^ ")")
  | _ -> "/* unsupported expr */"

let string_of_type = function
  | TypeInteger -> "int32_t"
  | TypeBoolean -> "bool"
  | TypeString -> "const char*"

let gen_stmt = function
  | Assign (name, expr) ->
      sprintf("    self->%s = %s;\n") name (string_of_expr expr)
  | Print expr ->
      sprintf("    printf(\"%%s\\n\", %s);\n") (string_of_expr expr)
  | _ -> "    /* unsupported stmt */\n"

let gen_routine class_name r cls_fields =
  let param_list =
    String.concat ", " (List.map (fun p ->
      sprintf "%s %s" (string_of_type p.param_type) p.param_name
    ) r.params)
  in
  let buffer = Buffer.create 256 in

  Buffer.add_string buffer (sprintf "void %s(%s* self, %s) {\n" r.name class_name param_list);

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

  List.iter (fun s -> Buffer.add_string buffer (gen_stmt s)) r.body;

  List.iter (fun e ->
    Buffer.add_string buffer (sprintf "    if (!(%s)) {\n" (string_of_expr e));
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
          Buffer.add_string buf (sprintf "void %s(%s* self%s%s);\n"
            r.name
            class_name
            (if params = "" then "" else ", ")
            params)
      | _ -> ()
    ) cls.features;
    Buffer.add_string buf ("\n#endif // RIVAR_H\n");
    Buffer.contents buf

let gen_class cls =
  let class_name = String.uppercase_ascii cls.class_name in
  let buf = Buffer.create 256 in

  Buffer.add_string buf "#include <stdio.h>\n#include <stdlib.h>\n#include <stdbool.h>\n#include <stdint.h>\n#include <string.h>\n\n";
  Buffer.add_string buf (sprintf "typedef struct {
");
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