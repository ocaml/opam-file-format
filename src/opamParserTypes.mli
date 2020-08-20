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
module Common: sig

  (** Relational operators *)
  type relop = [ `Eq  (** [=] *)
               | `Neq (** [!=] *)
               | `Geq (** [>=] *)
               | `Gt  (** [>] *)
               | `Leq (** [<=] *)
               | `Lt  (** [<] *)
               ]

  (** Logical operators *)
  type logop = [ `And (** [&] *) | `Or (** [|] *) ]

  (** Prefix operators *)
  type pfxop = [ `Not (** [!] *) | `Defined (** [?] *) ]

  type file_name = string

  (** Source file positions: [(filename, line, column)] *)
  type pos = file_name * int * int

  (** Environment variable update operators *)
  type env_update_op = Eq       (** [=] *)
                     | PlusEq   (** [+=] *)
                     | EqPlus   (** [=+] *)
                     | ColonEq  (** [:=] *)
                     | EqColon  (** [=:] *)
                     | EqPlusEq (** [=+=] *)
end

module FullPos : sig

  (** Source file positions *)
  type file_name = Common.file_name
  type pos =
    { filename: file_name;
      start: int * int; (* line, column *)
      stop: int * int; (* line, column *)
    }
  type 'a with_pos =
    { pelem : 'a;
      pos : pos
    }

  type relop_kind = Common.relop
  and rel_op = relop_kind with_pos

  type logop_kind = Common.logop
  and log_op = logop_kind with_pos

  type pfxop_kind = Common.pfxop
  and pfx_op = pfxop_kind with_pos

  type env_update_op_kind = Common.env_update_op
  and env_update_op = env_update_op_kind with_pos

  (** Base values *)
  type value_kind =
    | Bool of bool
    | Int of int
    | String of string
    | Relop of rel_op * value * value
    | Prefix_relop of rel_op * value
    | Logop of log_op * value * value
    | Pfxop of pfx_op * value
    | Ident of string
    | List of value list with_pos
    | Group of value list with_pos
    | Option of value * value list with_pos
    | Env_binding of value * env_update_op * value
  and value = value_kind with_pos

  type opamfile_section =
    { section_kind  : string with_pos;
      section_name  : string with_pos option;
      section_items : opamfile_item list with_pos;
    }
  and opamfile_item_kind =
    | Section of opamfile_section
    | Variable of string with_pos * value
  and opamfile_item = opamfile_item_kind with_pos

  type opamfile = {
    file_contents: opamfile_item list;
    file_name    : file_name;
  }

end

include module type of struct include Common end

(** Base values *)
type value =
  | Bool of pos * bool
  (** [bool] atoms *)
  | Int of pos * int
  (** [int] atoms *)
  | String of pos * string
  (** [string] atoms *)
  | Relop of pos * relop * value * value
  (** Relational operators with two values (e.g. [os != "win32"]) *)
  | Prefix_relop of pos * relop * value
  (** Relational operators in prefix position (e.g. [< "4.07.0"]) *)
  | Logop of pos * logop * value * value
  (** Logical operators *)
  | Pfxop of pos * pfxop * value
  (** Prefix operators *)
  | Ident of pos * string
  (** Identifiers *)
  | List of pos * value list
  (** Lists of values ([[x1 x2 ... x3]]) *)
  | Group of pos * value list
  (** Groups of values ([(x1 x2 ... x3)]) *)
  | Option of pos * value * value list
  (** Value with optional list ([x1 {x2 x3 x4}]) *)
  | Env_binding of pos * value * env_update_op * value
  (** Environment variable binding ([FOO += "bar"]) *)

(** An opamfile section *)
type opamfile_section = {
  section_kind  : string;            (** Section kind (e.g. [extra-source]) *)
  section_name  : string option;     (** Section name (e.g. ["myfork.patch"]) *)
  section_items : opamfile_item list (** Content of the section *);
}

(** An opamfile is composed of sections and variable definitions *)
and opamfile_item =
  | Section of pos * opamfile_section (** e.g. [kind ["name"] { ... }] *)
  | Variable of pos * string * value  (** e.g. [opam-version: "2.0"] *)

(** A file is a list of items and the filename *)
type opamfile = {
  file_contents: opamfile_item list; (** Content of the file *)
  file_name    : file_name;          (** Name of the disk file this record was
                                         loaded from *)
}
