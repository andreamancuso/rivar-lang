%{
open Ast

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

%left OR
%left AND
%nonassoc EQ NEQ
%nonassoc LT GT LE GE
%left PLUS MINUS
%left STAR SLASH

%token <string> IDENT
%token CLASS END FEATURE COLON
%token TYPE_INTEGER TYPE_BOOLEAN
%token TYPE_STRING
%token REQUIRE DO ENSURE
%token LPAREN RPAREN
%token PLUS MINUS STAR SLASH
%token GT LT EQ NEQ
%token LE GE
%token ASSIGN
%token OLD
%token AND OR
%token EOF
%token COMMA
%token PRINT
%token ARROW
%token THIS
%token RETURN
%token <string> STRINGLIT
%token <string> INT

%start <Ast.program> program
%type <Ast.class_decl> class_decl
%type <Ast.class_decl list> class_decl_list
%type <Ast.feature_decl list> feature_list
%type <Ast.feature_decl> feature_decl
%type <Ast.expr> expr
%type <Ast.expr list> expr_list
%type <Ast.stmt list> stmt_list
%type <Ast.stmt> stmt
%type <Ast.param> param
%type <Ast.param list> param_list
%type <Ast.type_expr> type_expr
%type <Ast.routine_body> routine_body
%type <Ast.feature_decl list> feature_block
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
  | IDENT LPAREN param_list RPAREN COLON type_expr routine_body {
      let body_block = $7 in
      Routine {
        name = $1;
        params = $3;
        return_type = Some $6;
        require = body_block.req;
        body = body_block.body;
        ensure = body_block.ens;
      }
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
  | DO stmt_list END {
      ({ req = []; body = $2; ens = [] } : Ast.routine_body)
  }
  | REQUIRE expr_list DO stmt_list END {
      ({ req = $2; body = $4; ens = [] } : Ast.routine_body)
  }
  | DO stmt_list ENSURE expr_list END {
      ({ req = []; body = $2; ens = $4 } : Ast.routine_body)
  }
  | REQUIRE expr_list DO stmt_list ENSURE expr_list END {
      ({ req = $2; body = $4; ens = $6 } : Ast.routine_body)
  }


expr_list:
  | expr { [$1] }
  | expr expr_list { $1 :: $2 }

stmt_list:
  | stmt { [$1] }
  | stmt stmt_list { $1 :: $2 }

stmt:
  | IDENT MINUS ASSIGN expr { 
      let var = Var (VarEnv.classify $1, $1) in
      Assign (var, BinOp (Sub, var, $4)) 
    }
  | IDENT PLUS ASSIGN expr { 
      let var = Var (VarEnv.classify $1, $1) in
      Assign (var, BinOp (Add, var, $4)) 
    }
  | IDENT STAR ASSIGN expr { 
      let var = Var (VarEnv.classify $1, $1) in
      Assign (var, BinOp (Mul, var, $4)) 
    }
  | IDENT SLASH ASSIGN expr { 
      let var = Var (VarEnv.classify $1, $1) in
      Assign (var, BinOp (Div, var, $4)) 
    }
  | IDENT ASSIGN expr { Assign(Var(VarEnv.classify $1, $1), $3) }
  | THIS ARROW IDENT ASSIGN expr { Assign(Var(Field, $3), $5) }
  | PRINT LPAREN expr RPAREN { Print($3) }
  | RETURN expr            { Return (Some $2) }
  | RETURN { Return None }

expr:
  | INT { IntLit(int_of_string $1) }
  | IDENT { Var (VarEnv.classify $1, $1) }
  | OLD THIS ARROW IDENT { Old($4) }
  | STRINGLIT { StringLit($1) }
  | expr PLUS expr { BinOp(Add, $1, $3) }
  | expr MINUS expr { BinOp(Sub, $1, $3) }
  | expr STAR expr { BinOp(Mul, $1, $3) }
  | expr SLASH expr { BinOp(Div, $1, $3) }
  | expr GT expr { BinOp(Gt, $1, $3) }
  | expr LT expr { BinOp(Lt, $1, $3) }
  | expr GE expr { BinOp(Ge, $1, $3) }
  | expr LE expr { BinOp(Le, $1, $3) }
  | expr EQ expr { BinOp(Eq, $1, $3) }
  | expr NEQ expr { BinOp(Neq, $1, $3) }
  | expr AND expr { BinOp(And, $1, $3) }
  | expr OR expr  { BinOp(Or, $1, $3) }
  | THIS ARROW IDENT { Var(Field, $3) }

type_expr:
  | TYPE_INTEGER { TypeInteger }
  | TYPE_BOOLEAN { TypeBoolean }
  | TYPE_STRING { TypeString }
