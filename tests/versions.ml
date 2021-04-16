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
  "opam-version > 2.0 not at start 1",
  {|
version: "2.1"
opam-version: "2.1"
  |};
  "opam-version > 2.1 repeated",
  {|
opam-version: "2.1"
opam-version: "2.1"
  |};
  "no opam-version and parsing error",
  {|
build: [ "echo"
  |};
  "opam-version 2.1 and parsing error",
  {|
opam-version: "2.1"
build: [ "echo"
  |};
] |> List.map (fun (name, content) ->
    name, (fun () ->
        A.check_raises name Parsing.Parse_error (fun () ->
            OpamParser.FullPos.string content "broken.opam" |> ignore)))

let tests_noexn = [
  "opam-version 2.2 and parsing error",
  {|
opam-version: "2.2"
build: [ "echo"
  |};
] |> List.map (fun (name, content) ->
    name, (fun () ->
        A.check A.unit name ()
          (OpamParser.FullPos.string content "broken.opam" |> ignore)))

let tests =
  ["opam-version", tests_exn @ tests_noexn]
