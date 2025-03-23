open Ast

let () =
  let filename = Sys.argv.(1) in
  let in_chan = open_in filename in
  let lexbuf = Lexing.from_channel in_chan in
  let ast =
    try
      Parser.program Lexer.token lexbuf
    with
    | Parser.Error ->
        let pos = lexbuf.Lexing.lex_curr_p in
        Printf.eprintf "Parse error at line %d, column %d\n"
          pos.pos_lnum
          (pos.pos_cnum - pos.pos_bol);
        exit 1
  in
  close_in in_chan;

  List.iter (fun cls ->
    Printf.printf "Parsed class: %s\n" cls.class_name;
    List.iter (function
      | Field(name, TypeInteger) ->
          Printf.printf "  Field: %s : INTEGER\n" name
      | Field(name, TypeBoolean) ->
          Printf.printf "  Field: %s : BOOLEAN\n" name
      | Routine _ ->
          Printf.printf "  [Routine: not shown yet]\n"
    ) cls.features
  ) ast
