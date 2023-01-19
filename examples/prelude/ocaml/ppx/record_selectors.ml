open Ppxlib
module List = ListLabels
open Ast_builder.Default

let accessor_impl (ld : label_declaration) =
  let loc = ld.pld_loc in
  pstr_value
    ~loc
    Nonrecursive
    [
      {
        pvb_pat = ppat_var ~loc ld.pld_name;
        pvb_expr =
          pexp_fun
            ~loc
            Nolabel
            None
            (ppat_var ~loc { loc; txt = "x" })
            (pexp_field
               ~loc
               (pexp_ident ~loc { loc; txt = lident "x" })
               { loc; txt = lident ld.pld_name.txt });
        pvb_attributes = [];
        pvb_loc = loc;
      };
    ]

let accessor_intf ~ptype_name (ld : label_declaration) =
  let loc = ld.pld_loc in
  psig_value
    ~loc
    {
      pval_name = ld.pld_name;
      pval_type =
        ptyp_arrow
          ~loc
          Nolabel
          (ptyp_constr ~loc { loc; txt = lident ptype_name.txt } [])
          ld.pld_type;
      pval_attributes = [];
      pval_loc = loc;
      pval_prim = [];
    }

let generate_impl ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun (td : type_declaration) ->
      match td with
      | {
       ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open;
       ptype_loc;
       _;
      } ->
        let ext =
          Location.error_extensionf
            ~loc:ptype_loc
            "Cannot derive accessors for non record types"
        in
        [Ast_builder.Default.pstr_extension ~loc ext []]
      | { ptype_kind = Ptype_record fields; _ } ->
        List.map fields ~f:accessor_impl)
  |> List.concat

let generate_intf ~ctxt (_rec_flag, type_declarations) =
  let loc = Expansion_context.Deriver.derived_item_loc ctxt in
  List.map type_declarations ~f:(fun (td : type_declaration) ->
      match td with
      | {
       ptype_kind = Ptype_abstract | Ptype_variant _ | Ptype_open;
       ptype_loc;
       _;
      } ->
        let ext =
          Location.error_extensionf
            ~loc:ptype_loc
            "Cannot derive accessors for non record types"
        in
        [Ast_builder.Default.psig_extension ~loc ext []]
      | { ptype_kind = Ptype_record fields; ptype_name; _ } ->
        List.map fields ~f:(accessor_intf ~ptype_name))
  |> List.concat

let impl_generator = Deriving.Generator.V2.make_noarg generate_impl
let intf_generator = Deriving.Generator.V2.make_noarg generate_intf
let _ = Deriving.add "record_selectors" ~str_type_decl:impl_generator ~sig_type_decl:intf_generator
