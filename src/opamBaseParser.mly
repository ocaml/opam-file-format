/**************************************************************************/
/*                                                                        */
/*    Copyright 2012-2017 OCamlPro                                        */
/*    Copyright 2012 INRIA                                                */
/*                                                                        */
/*  All rights reserved. This file is distributed under the terms of the  */
/*  GNU Lesser General Public License version 2.1, with the special       */
/*  exception on linking described in the file LICENSE.                   */
/*                                                                        */
/**************************************************************************/

%{

open OpamParserTypes.FullPos

(** Opam config file generic type parser *)

let get_pos_full ?(s=1) n =
  let spos = Parsing.rhs_start_pos s in
  let epos = Parsing.rhs_end_pos n in
  Lexing.({
      filename = spos.pos_fname;
      start = spos.pos_lnum, spos.pos_cnum - spos.pos_bol;
      stop = epos.pos_lnum, epos.pos_cnum - epos.pos_bol;
    })

let get_pos n = get_pos_full ~s:n n

let parsed_so_far = ref []

let record_token t =
  parsed_so_far := t :: !parsed_so_far; t

(* This must match up with the package's version; checked by the build system *)
let version = (2, 1)

%}

%token <string> STRING IDENT
%token <bool> BOOL
%token EOF
%token LBRACKET RBRACKET
%token LPAR RPAR
%token LBRACE RBRACE
%token COLON
%token <int> INT
%token <OpamParserTypes.FullPos.relop_kind> RELOP
%token AND
%token OR
%token <OpamParserTypes.FullPos.pfxop_kind> PFXOP
%token <OpamParserTypes.FullPos.env_update_op_kind> ENVOP

%left COLON
%left ATOM
%left OR
%left AND
%nonassoc ENVOP
%nonassoc PFXOP
%left LBRACE RBRACE
%nonassoc RELOP
%nonassoc URELOP

%start main value
%type <string -> OpamParserTypes.FullPos.opamfile> main
%type <OpamParserTypes.FullPos.value> value
%type <OpamParserTypes.FullPos.value list> values
%type <OpamParserTypes.FullPos.opamfile_item> item

%%

main:
| items EOF { parsed_so_far := []; fun file_name ->
        { file_contents = $1; file_name } }
;

items:
| item items { $1 :: $2 }
|            { [] }
;

item:
| IDENT COLON value                {
  record_token
  { pos = get_pos_full 3;
    pelem =
      Variable ({ pos = get_pos 1; pelem =  $1 }, $3);
  }
}
| IDENT LBRACE items RBRACE {
  record_token
  { pos = get_pos_full 4;
    pelem =
      Section ({section_kind = { pos = get_pos 1; pelem = $1 };
                section_name = None;
                section_items =
                  { pos = get_pos_full ~s:2 4; pelem = $3 };
               })
  }
}
| IDENT STRING LBRACE items RBRACE {
  record_token
  { pos = get_pos_full 4;
    pelem =
      Section ({section_kind = { pos = get_pos 1; pelem = $1 };
                section_name = Some { pos = get_pos 2; pelem = $2 };
                section_items =
                  { pos = get_pos_full ~s:3 5; pelem = $4 };
               })
  }
}
;

value:
| atom            %prec ATOM { $1 }
| LPAR values RPAR           {{ pos = get_pos_full 3 ; pelem = Group { pos = get_pos_full ~s:1 3; pelem = $2 } }}
| LBRACKET values RBRACKET   {{ pos = get_pos_full 3 ; pelem = List { pos = get_pos_full ~s:1 3; pelem = $2 } }}
| value LBRACE values RBRACE {{ pos = get_pos_full 4 ;
                                pelem = Option ($1, { pos = get_pos_full ~s:2 4; pelem = $3 }) }}
| value AND value            {{ pos = get_pos_full 3 ; pelem = Logop ({ pos = get_pos 2 ; pelem = `And },$1,$3) }}
| value OR value             {{ pos = get_pos_full 3 ; pelem = Logop ({ pos = get_pos 2 ; pelem = `Or },$1,$3) }}
| atom RELOP atom            {{ pos = get_pos_full 3 ; pelem = Relop ({ pos = get_pos 2 ; pelem = $2 },$1,$3) }}
| atom ENVOP atom            {{ pos = get_pos_full 3 ; pelem = Env_binding ($1,{ pos = get_pos 2 ; pelem = $2 },$3) }}
| PFXOP value                {{ pos = get_pos_full 2 ; pelem = Pfxop ({ pos = get_pos 1 ; pelem = $1 },$2) }}
| RELOP atom                 {{ pos = get_pos_full 2 ; pelem = Prefix_relop ({ pos = get_pos 1 ; pelem = $1 },$2) }}
;

values:
|                            { [] }
| value values               { $1 :: $2 }
;

atom:
| IDENT                      {{ pos = get_pos 1 ; pelem = Ident $1 }}
| BOOL                       {{ pos = get_pos 1 ; pelem = Bool $1 }}
| INT                        {{ pos = get_pos 1 ; pelem = Int $1 }}
| STRING                     {{ pos = get_pos 1 ; pelem = String $1 }}
;

%%

let nopatch v =
  let s =
  try
    let i = String.index v '.' in
    let i = String.index_from v (i+1) '.' in
    (String.sub v 0 i)
  with Not_found ->
    let rec f i =
      if i >= String.length v then v
      else match String.get v i with
        | '0'..'9' | '.' -> f (i+1)
        | _ -> String.sub v 0 i
    in
    f 0
  in
    try Scanf.sscanf s "%u.%u" (fun maj min -> (maj, min))
    with Scanf.Scan_failure _ -> (0, 0)

let with_clear_parser f x =
  try
    let r = f x in
    Parsing.clear_parser ();
    r
  with e ->
    Parsing.clear_parser ();
    raise e

exception Nothing

let main t l file_name =
  (* Always return a result from parsing/lexing, but note if an exception
     occurred. *)
  let parsing_exception = ref Nothing in
  let t l =
    try t l
    with
    | Sys.Break
    | Assert_failure _
    | Match_failure _ as e -> raise e
    | e -> parsing_exception := e; EOF
  in
    let r =
      try with_clear_parser (main t l) file_name
      with Parsing.Parse_error as e ->
        parsing_exception := e;
        (* Record the tokens captured so far *)
        let r = {file_contents = List.rev !parsed_so_far; file_name} in
        parsed_so_far := [];
        r
    in
    match r with
    | {file_contents = {pelem = Variable({pelem = "opam-version"; _}, {pelem = String ver; _}); _}::items; _}
      when nopatch ver >= (2, 1) ->
        let opam_version_variable = function
        | {pelem = Variable({pelem = "opam-version"; _}, _); _} -> true
        | _ -> false
        in
          (* For opam-version: 2.1 and later, there must be no other opam-version
             fields. *)
          if List.exists opam_version_variable items then
            raise Parsing.Parse_error;
          (* Parsing and lexing errors from future versions of opam are ignored:
             the intent is that the tool will abort/ignore because of the
             opam-version field rather than through lexer/parser errors. *)
          if !parsing_exception != Nothing && nopatch ver <= version then
            raise !parsing_exception;
          r
    | {file_contents = items; _} ->
        let opam_version_greater_2_0 = function
        | {pelem = Variable({pelem = "opam-version"; _}, {pelem = String ver; _}); _} ->
            nopatch ver > (2, 0)
        | _ -> false
        in
          (* opam-version: 2.1 or later must be the first item. *)
          if List.exists opam_version_greater_2_0 items then
            raise Parsing.Parse_error;
          (* If no opam-version field was given, all exceptions must be
             raised. *)
          if !parsing_exception != Nothing then
            raise !parsing_exception;
          r

let value t l =
  try
    let r = value t l in
    Parsing.clear_parser ();
    r
  with
  | e ->
    Parsing.clear_parser ();
    raise e
