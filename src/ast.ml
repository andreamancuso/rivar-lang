type type_expr =
  | TypeInteger
  | TypeBoolean
  | TypeString

type var_context =
  | Local
  | Param
  | Field

type bin_op =
  | Add | Sub | Mul | Div
  | Eq | Neq | Gt | Lt | Ge | Le
  | And | Or

type unary_op =
  | Not | Neg

type expr =
  | IntLit of int
  | BoolLit of bool
  | StringLit of string
  | Var of var_context * string
  | BinOp of bin_op * expr * expr
  | UnaryOp of unary_op * expr
  | Call of expr * string * expr list
  | Old of string

type stmt =
  | Assign of expr * expr
  | If of expr * stmt list * stmt list option
  | While of expr * stmt list
  | Return of expr option
  | Print of expr

type param = {
  param_name : string;
  param_type : type_expr;
}

type routine = {
  name : string;
  params : param list;
  return_type : type_expr option;
  require : expr list;
  body : stmt list;
  ensure : expr list;
}

type routine_body = {
  req : expr list;
  body : stmt list;
  ens : expr list;
}


type feature_decl =
  | Field of string * type_expr
  | Routine of routine

type class_decl = {
  class_name : string;
  features : feature_decl list;
}

type program = class_decl list
