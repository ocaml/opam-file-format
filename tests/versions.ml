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

let tests = [
  (let n = "opam-version > 2.0 not at start 1" in
   n, (fun () -> A.check_raises n Parsing.Parse_error (fun () -> OpamParser.FullPos.string "version: \"2.1\"\nopam-version: \"2.1\"" "broken.opam" |> ignore)));
  (let n = "opam-version > 2.1 repeated" in
   n, (fun () -> A.check_raises n Parsing.Parse_error (fun () -> OpamParser.FullPos.string "opam-version: \"2.1\"\nopam-version: \"2.1\"" "broken.opam" |> ignore)));
]

let tests =
  ["opam-version", tests]
