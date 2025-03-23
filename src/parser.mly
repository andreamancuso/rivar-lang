%{
open Ast
%}

%token <string> IDENT
%token CLASS END FEATURE COLON
%token TYPE_INTEGER TYPE_BOOLEAN
%token EOF

%start <Ast.program> program

%%

program:
  | class_decl_list EOF { $1 }

class_decl_list:
  | class_decl { [$1] }
  | class_decl class_decl_list { $1 :: $2 }

class_decl:
  | CLASS IDENT feature_block END {
      {
        class_name = $2;
        features = $3;
      }
    }

feature_block:
  | FEATURE feature_list { $2 }

feature_list:
  | field_decl { [$1] }
  | field_decl feature_list { $1 :: $2 }

field_decl:
  | IDENT COLON type_expr { Field($1, $3) }

type_expr:
  | TYPE_INTEGER { TypeInteger }
  | TYPE_BOOLEAN { TypeBoolean }
