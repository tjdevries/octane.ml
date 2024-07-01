module I = Parser.MenhirInterpreter

let succeed v = Ok v

let fail lexbuf _ =
  let msg =
    Fmt.str "At offset %d: syntax error.\n%!" (Lexing.lexeme_start lexbuf)
  in
  Error msg
;;

let loop lexbuf result =
  let rec skip_comments buf =
    match Lexer.read buf with
    | COMMENT -> skip_comments buf
    | token ->
      (* Fmt.pr "Handling token: %a@." Tokenize.pp_token token; *)
      token
  in
  let supplier = I.lexer_lexbuf_to_supplier skip_comments lexbuf in
  I.loop_handle succeed (fail lexbuf) supplier result
;;

let parse_with incremental s =
  let lexbuf = Lexing.from_string s in
  let parser = incremental lexbuf.lex_curr_p in
  loop lexbuf parser
;;

let parse s = parse_with Parser.Incremental.query s
