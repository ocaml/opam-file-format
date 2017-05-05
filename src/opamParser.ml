(**************************************************************************)
(*                                                                        *)
(*    Copyright 2016 OCamlPro                                             *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

let main = OpamBaseParser.main

let string str filename =
  let lexbuf = Lexing.from_string str in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  OpamBaseParser.main OpamLexer.token lexbuf filename

let channel ic filename =
  let lexbuf = Lexing.from_channel ic in
  lexbuf.Lexing.lex_curr_p <-
    { lexbuf.Lexing.lex_curr_p with Lexing.pos_fname = filename };
  OpamBaseParser.main OpamLexer.token lexbuf filename

let file filename =
  let ic = open_in filename in
  try channel ic filename
  with e -> close_in ic; raise e

let value_of_string ?pos str =
  let lexbuf = Lexing.from_string str in
  begin match pos with
    | Some pos -> lexbuf.Lexing.lex_curr_p <- pos
    | None -> ()
  end;
  try
    let v = OpamBaseParser.value OpamLexer.token lexbuf in
    Parsing.clear_parser ();
    v
  with e -> Parsing.clear_parser (); raise e
