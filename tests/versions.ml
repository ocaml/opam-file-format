(**************************************************************************)
(*                                                                        *)
(*    Copyright 2021 David Allsopp Ltd.                                   *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

module A = Alcotest

let tests_exn = [
  "opam-version > 2.0 not at start 1", OpamLexer.Error("opam-version must be the first non-comment line"),
  {|
version: "2.1"
opam-version: "2.1"
  |};
  "opam-version > 2.1 repeated", OpamLexer.Error("opam-version cannot be repeated"),
  {|
opam-version: "2.1"
opam-version: "2.1"
  |};
  "no opam-version and parsing error", Parsing.Parse_error,
  {|
build: [ "echo"
  |};
  "opam-version 2.1 and lexing error", OpamLexer.Error "'@' is not a valid token",
  {|
opam-version: "2.1"
@
  |};
  "opam-version 2.1 and parsing error", Parsing.Parse_error,
  {|
opam-version: "2.1"
build: [ "echo"
  |};
  "opam-version 2.1 and immediate parsing error", Parsing.Parse_error,
  {|
opam-version: "2.1"
!!
  |};
] |> List.map (fun (name, exn, content) ->
    name, (fun () ->
        A.check_raises name exn (fun () ->
            OpamParser.FullPos.string content "broken.opam" |> ignore)))

let has_sentinel =
  let open OpamParserTypes.FullPos in
  fun {file_contents; _} ->
    match List.rev file_contents with
    | {pelem = Section {section_kind = {pelem = "#"; _}; _}; _}::_ -> true
    | _ -> false

let tests_noexn = [
  "opam-version 42.0 and parsing error",
  {|
opam-version: "42.0"
version: "42.0"
!!
  |};
  "opam-version 42.0 and evil parsing error",
  {|
opam-version: "42.0" <
  |};
  "opam-version 42.0 and immediate parsing error",
  {|
opam-version: "42.0"
!!
  |};
  "opam-version 42.0 and lexing error",
  {|
opam-version: "42.0"
@
  |};
] |> List.map (fun (name, content) ->
    name, (fun () ->
        A.check A.bool name true
          (OpamParser.FullPos.string content "broken.opam"
           |> has_sentinel)))

let tests =
  ["opam-version", tests_exn @ tests_noexn]
