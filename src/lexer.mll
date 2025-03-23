{
open Parser
}

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | "class"    { CLASS }
  | "end"      { END }
  | "feature"  { FEATURE }
  | "require"  { REQUIRE }
  | "do"       { DO }
  | "ensure"   { ENSURE }
  | "INTEGER"  { TYPE_INTEGER }
  | "BOOLEAN"  { TYPE_BOOLEAN }
  | "old"      { OLD }
  | ":"        { COLON }
  | ":="       { ASSIGN }
  | "+"        { PLUS }
  | "-"        { MINUS }
  | "*"        { STAR }
  | "/"        { SLASH }
  | "="        { EQ }
  | "!="       { NEQ }
  | ">"        { GT }
  | "<"        { LT }
  | "("        { LPAREN }
  | ")"        { RPAREN }
  | ","        { COMMA }
  | ['0'-'9']+ as i { INT i }
  | ['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''0'-'9''_']* as id { IDENT id }
  | eof        { EOF }
