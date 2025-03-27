{
open Parser
}

rule token = parse
  | [' ' '\t' '\r'] { token lexbuf }
  | '\n'        { Lexing.new_line lexbuf; token lexbuf }
  | "class"     { CLASS }
  | "end"       { END }
  | "feature"   { FEATURE }
  | "require"   { REQUIRE }
  | "do"        { DO }
  | "ensure"    { ENSURE }
  | "INTEGER"   { TYPE_INTEGER }
  | "BOOLEAN"   { TYPE_BOOLEAN }
  | "STRING"    { TYPE_STRING }
  | "old"       { OLD }
  | "and"       { AND }
  | "or"        { OR }
  | "print"     { PRINT }
  | ":"         { COLON }
  | "="         { ASSIGN }
  | "this"      { THIS }
  | "->"        { ARROW }
  | "+"         { PLUS }
  | "-"         { MINUS }
  | "*"         { STAR }
  | "/"         { SLASH }
  | "=="        { EQ }
  | "!="        { NEQ }
  | ">"         { GT }
  | "<"         { LT }
  | "<="        { LE }
  | ">="        { GE }
  | "("         { LPAREN }
  | ")"         { RPAREN }
  | ","         { COMMA }
  | ['0'-'9']+ as i { INT i }
  | '"' [^'"']* '"' as s { STRINGLIT (String.sub s 1 (String.length s - 2)) }
  | ['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''0'-'9''_']* as id { IDENT id }
  | eof        { EOF }
