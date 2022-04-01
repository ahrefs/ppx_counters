(** Generate useful code for stats gathering from records of counters
See generated code with :
  ocamlc -dsource ../_build/default/orig.pp.ml
*)

open Ppxlib

(* lazy *)
open Ast_builder.Make(struct let loc = Location.none end)

let lid = Located.lident
let record l = pexp_record l None

let get_type_constr1 t =
  match t.ptyp_desc with
  | Ptyp_constr ({txt=Longident.Lident s; _}, tl) -> Some (s, tl)
  | _ -> None

let rec get_type_constr t =
  match get_type_constr1 t with
  | Some (s, [t]) -> s :: get_type_constr t
  | Some (s, []) -> [s]
  | _ -> []

let attr_name =
  Attribute.declare "name" Attribute.Context.Label_declaration
    Ast_pattern.(single_expr_payload (estring __))
    (fun x -> x)

type kind = Ref | Mut | Opt
type field = { field : string; name : string; kind : kind; }

let make_field ({pld_name; pld_loc; pld_type; pld_mutable; _} as ld)  =
  (* check for name override *)
  let name = Option.value ~default:pld_name.txt @@ Attribute.get attr_name ld in
  let kind =
    match pld_mutable, get_type_constr pld_type with
    | Immutable, ["ref"; "int"] -> Ref
    | Mutable, ["int"] -> Mut
    | Mutable, ["option"; "int"] -> Opt
    | _ -> Location.raise_errorf ~loc:pld_loc "unsupported field type"
  in
  { field = pld_name.txt; name; kind; }

let access f r =
  let field = pexp_field (evar r) (lid f.field) in
  match f.kind with
  | Ref -> pexp_field field (lid "contents")
  | Mut -> field
  | Opt -> [%expr Option.value ~default:0 [%e field]]

let create f v =
  lid f.field,
  match f.kind with
  | Ref -> [%expr ref [%e v]]
  | Mut -> v
  | Opt -> [%expr Some [%e v]]

let make_str name lambda = pstr_value Nonrecursive [value_binding ~pat:(pvar name) ~expr:lambda]

let gen_fresh fields =
  let fields = fields |> List.map (fun fld -> create fld (eint 0)) in
  [%stri let fresh () = [%e record fields]]
let gen_export fields =
  let fields = fields |> List.map (fun fld -> pexp_tuple [estring fld.name; access fld "x"]) in
  [%stri let export x = [%e elist fields] ]
let gen_sub fields =
  let fields = fields |> List.map begin fun fld ->
    create fld [%expr [%e access fld "a"] - [%e access fld "b"]]
  end in
  [%stri let sub a b = [%e record fields]]
let gen_copy fields =
  let fields = fields |> List.map (fun fld -> create fld (access fld "x")) in
  [%stri let copy x = [%e record fields]]

let generate loc name fields =
    let name = Ast_builder.Default.Located.mk ~loc (Some (String.capitalize_ascii name)) in
(*     let mut = match find_attr_expr "counters" tdecl.ptype_attributes with Some x -> get_lid x = Some "mut" | None -> false in *)
    let fields = List.map make_field fields in
    let str_items = [
      gen_fresh fields;
      gen_export fields;
      gen_sub fields;
      gen_copy fields;
    ] in
    let mb = module_binding ~name ~expr:(pmod_structure str_items) in
    [ pstr_module mb ]

let generate_impl ~ctxt (_rec_flag, type_decls) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  type_decls |> List.map begin fun (td : type_declaration) ->
      match td with
      | { ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open; _ } ->
          Location.raise_errorf ~loc "Cannot derive counters for non record types"
      | { ptype_kind = Ptype_record fields; ptype_name; _ } -> generate loc ptype_name.txt fields
    end
  |> List.concat

let (_:Deriving.t) = Deriving.add "counters" ~str_type_decl:(Deriving.Generator.V2.make_noarg generate_impl)
(*     ~sig_type_decl:intf_generator *)