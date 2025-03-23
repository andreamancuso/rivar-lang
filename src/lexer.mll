{
open Parser
}

rule token = parse
  | [' ' '\t' '\r' '\n'] { token lexbuf }
  | "class"    { CLASS }
  | "end"      { END }
  | "feature"  { FEATURE }
  | ":"        { COLON }
  | "INTEGER"  { TYPE_INTEGER }
  | "BOOLEAN"  { TYPE_BOOLEAN }
  | ['A'-'Z''a'-'z''_']['A'-'Z''a'-'z''0'-'9''_']* as id { IDENT id }
  | eof        { EOF }
