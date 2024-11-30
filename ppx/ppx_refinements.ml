open Ppxlib

(* Helper function for the refinement extension *)
let node_builder ~loc ~path:_ var var_type predicate =
  (* Attach refinement as an attribute *)
  let (module B) = Ast_builder.make loc in
  let open B in
  (*
       We store additional information about the type i.e. its
       refinements as attributes.
    *)

  (* Extract the name from Longident.t loc *)
  let var_name =
    match var with
    | Longident.Lident name -> name (* Extract plain name, e.g., "x" *)
    | _ ->
        Location.raise_errorf ~loc
          "Expected an identifier, cannot extract .name"
  in

  let payload =
    pstr_value Nonrecursive
      [ value_binding ~pat:(pvar var_name) ~expr:predicate ]
  in
  {
    var_type with
    ptyp_attributes =
      attribute ~name:{ txt = "refinement"; loc } ~payload:(PStr [ payload ])
      :: var_type.ptyp_attributes;
  }

(* In place of type annotations, users can express
   refinement types as: [%refinement (<varname>:<type>), <predicate>]

   For example:
   let x = 10; could be written as
   let x : [%refinement (v:int), v > 0] = 10
   Note that
   let x : [%refinement (v:int, v > 0)] = 10
   is not valid, as the (name:type) binding have to be inside parenthsis in OCaml

   The above example would generate verification condition perhaps like:
   v = 10 => v > 0

   Or
   type nat = int
   could be written as:
   type nat = [%refinement (v:int, v > 0)]

   Or
   let sum a b = a + b;
   could be written as:
   let sum (a : int), (b : int) : [%refinement (v:int, v >= a && v >= b)] = a + b

   VC:
   a = var('int') and
   b = var('int') and
   v = a + b => (v >= a and v >= b)


   etc. etc.
*)

let refinement_extension =
  Extension.declare "refinement" Extension.Context.core_type
    Ast_pattern.(
      single_expr_payload
        (pexp_tuple
           (pexp_constraint (pexp_ident __) (* Match `x` *) __ (* Match `int` *)
           ^:: __ (* Match the predicate: `x > 5` *) ^:: nil)))
    node_builder

(* Register the extension node *)
let () =
  Driver.register_transformation "ppx_refinements"
    ~rules:[ Context_free.Rule.extension refinement_extension ]
