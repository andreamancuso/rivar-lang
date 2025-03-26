%{
open Ast
open Parser_support

module VarEnv = struct
  let params = ref []
  let fields = ref []

  let reset () = params := []; fields := []

  let add_param name = params := name :: !params
  let add_field name = fields := name :: !fields

  let classify name =
    if List.mem name !params then Param
    else if List.mem name !fields then Field
    else Local  (* fallback *)
end
%}

%token <string> IDENT
%token CLASS END FEATURE COLON
%token TYPE_INTEGER TYPE_BOOLEAN
%token REQUIRE DO ENSURE
%token LPAREN RPAREN
%token PLUS MINUS STAR SLASH
%token GT LT EQ NEQ
%token ASSIGN
%token OLD
%token EOF
%token COMMA
%token <string> INT

%start <Ast.program> program

%%

program:
  | class_decl_list EOF { $1 }

class_decl_list:
  | class_decl { [$1] }
  | class_decl class_decl_list { $1 :: $2 }

class_decl:
  | CLASS IDENT feature_block END {
      VarEnv.reset ();
      {
        class_name = $2;
        features = $3;
      }
    }

feature_block:
  | FEATURE feature_list { $2 }

feature_list:
  | feature_decl { [$1] }
  | feature_decl feature_list { $1 :: $2 }

feature_decl:
  | IDENT COLON type_expr {
      VarEnv.add_field $1;
      Field($1, $3)
  }
  | IDENT LPAREN param_list RPAREN routine_body {
      let body_block = $5 in
      Routine {
        name = $1;
        params = $3;
        return_type = None;
        require = body_block.req;
        body = body_block.body;
        ensure = body_block.ens;
      }
    }

param_list:
  | /* empty */ { [] }
  | param { [$1] }
  | param COMMA param_list { $1 :: $3 }

param:
  | IDENT COLON type_expr {
      VarEnv.add_param $1;
      { param_name = $1; param_type = $3 }
  }

routine_body:
  | REQUIRE expr_list DO stmt_list ENSURE expr_list END {
    { req = $2; body = $4; ens = $6 }
  }

expr_list:
  | expr { [$1] }
  | expr expr_list { $1 :: $2 }

stmt_list:
  | stmt { [$1] }
  | stmt stmt_list { $1 :: $2 }

stmt:
  | IDENT ASSIGN expr { Assign($1, $3) }

expr:
  | INT { IntLit(int_of_string $1) }
  | IDENT { Var (VarEnv.classify $1, $1) }
  | OLD IDENT { Old($2) }
  | expr PLUS expr { BinOp(Add, $1, $3) }
  | expr MINUS expr { BinOp(Sub, $1, $3) }
  | expr STAR expr { BinOp(Mul, $1, $3) }
  | expr SLASH expr { BinOp(Div, $1, $3) }
  | expr GT expr { BinOp(Gt, $1, $3) }
  | expr LT expr { BinOp(Lt, $1, $3) }
  | expr EQ expr { BinOp(Eq, $1, $3) }
  | expr NEQ expr { BinOp(Neq, $1, $3) }

type_expr:
  | TYPE_INTEGER { TypeInteger }
  | TYPE_BOOLEAN { TypeBoolean }
