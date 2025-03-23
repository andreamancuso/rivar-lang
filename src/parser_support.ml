(* parser_support.ml *)

type routine_block = {
  req : Ast.expr list;
  body : Ast.stmt list;
  ens : Ast.expr list;
}