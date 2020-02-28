(**************************************************************************)
(*                                                                        *)
(*    Copyright 2012-2020 OCamlPro                                        *)
(*    Copyright 2012 INRIA                                                *)
(*                                                                        *)
(*  All rights reserved. This file is distributed under the terms of the  *)
(*  GNU Lesser General Public License version 2.1, with the special       *)
(*  exception on linking described in the file LICENSE.                   *)
(*                                                                        *)
(**************************************************************************)

(** Defines the types for the opam format lexer and parser *)

(** Source file positions *)
type file_name = string
type pos =
  { filename: file_name;
    start: int * int; (* line, column *)
    stop: int * int; (* line, column *)
  }
type 'a with_pos =
  { pelem : 'a;
    pos : pos
  }

(** Relational operators *)
type relop_kind =
  [ `Eq  (** [=] *)
  | `Neq (** [!=] *)
  | `Geq (** [>=] *)
  | `Gt  (** [>] *)
  | `Leq (** [<=] *)
  | `Lt  (** [<] *)
  ]
and rel_op = relop_kind with_pos

(** Logical operators *)
type logop_kind = [ `And (** [&] *) | `Or (** [|] *) ]
and log_op = logop_kind with_pos

(** Prefix operators *)
type pfxop_kind = [ `Not (** [!] *) | `Defined (** [?] *) ]
and pfx_op = pfxop_kind with_pos

(** Environment variable update operators *)
type env_update_op_kind =
  | Eq       (** [=] *)
  | PlusEq   (** [+=] *)
  | EqPlus   (** [=+] *)
  | ColonEq  (** [:=] *)
  | EqColon  (** [=:] *)
  | EqPlusEq (** [=+=] *)
and env_update_op = env_update_op_kind with_pos

(** Base values *)
type value_kind =
  | Bool of bool
      (** [bool] atoms *)
  | Int of int
      (** [int] atoms *)
  | String of string
      (** [string] atoms *)
  | Relop of rel_op * value * value
      (** Relational operators with two values (e.g. [os != "win32"]) *)
  | Prefix_relop of rel_op * value
      (** Relational operators in prefix position (e.g. [< "4.07.0"]) *)
  | Logop of log_op * value * value
      (** Logical operators *)
  | Pfxop of pfx_op * value
      (** Prefix operators *)
  | Ident of string
      (** Identifiers *)
  | List of value list with_pos
      (** Lists of values ([[x1 x2 ... x3]]) *)
  | Group of value list with_pos
      (** Groups of values ([(x1 x2 ... x3)]) *)
  | Option of value * value list with_pos
      (** Value with optional list ([x1 {x2 x3 x4}]) *)
  | Env_binding of value * env_update_op * value
      (** Environment variable binding ([FOO += "bar"]) *)
and value = value_kind with_pos

(** An opamfile section *)
type opamfile_section =
  { section_kind  : string with_pos;            (** Section kind (e.g. [extra-source]) *)
    section_name  : string with_pos option;     (** Section name (e.g. ["myfork.patch"]) *)
    section_items : opamfile_item list with_pos (** Content of the section *);
  }

(** An opamfile is composed of sections and variable definitions *)
and opamfile_item_kind =
  | Section of opamfile_section          (** e.g. [kind ["name"] { ... }] *)
  | Variable of string with_pos * value  (** e.g. [opam-version: "2.0"] *)
and opamfile_item = opamfile_item_kind with_pos

(** A file is a list of items and the filename *)
type opamfile = {
  file_contents: opamfile_item list; (** Content of the file *)
  file_name    : file_name;          (** Name of the disk file this record was
                                         loaded from *)
}
