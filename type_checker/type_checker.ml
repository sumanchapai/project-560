open Z3
open Parsetree

let cfg = [("model", "true")];;
let ctx = Z3.mk_context cfg;;

let true_ = Boolean.mk_val ctx true

let _simple_z3_example _ =
  (* Create a Z3 configuration and context *)
  let cfg = [("model", "true")] in
  let ctx = Z3.mk_context cfg in

  (* Define an integer sort and two integer variables *)
  let int_sort = Arithmetic.Integer.mk_sort ctx in
  let x = Expr.mk_const ctx (Symbol.mk_string ctx "x") int_sort in
  let y = Expr.mk_const ctx (Symbol.mk_string ctx "y") int_sort in

  (* Create the constraint: x + y = 10 *)
  let x_plus_y = Arithmetic.mk_add ctx [x; y] in
  let constraint1 = Boolean.mk_eq ctx x_plus_y (Arithmetic.Integer.mk_numeral_i ctx 10) in

  (* Create the constraint: x > 0 *)
  let constraint2 = Arithmetic.mk_gt ctx x (Arithmetic.Integer.mk_numeral_i ctx 0) in

  (* Create the constraint: y > 0 *)
  let constraint3 = Arithmetic.mk_gt ctx y (Arithmetic.Integer.mk_numeral_i ctx 0) in

  (* Combine the constraints *)
  let combined_constraints = Boolean.mk_and ctx [constraint1; constraint2; constraint3] in

  (* Create a solver and add the constraints *)
  let solver = Solver.mk_solver ctx None in
  Solver.add solver [combined_constraints];

  (* Check satisfiability *)
  match Solver.check solver [] with
  | Solver.SATISFIABLE ->
      Printf.printf "The constraints are satisfiable.\n";
      (* Print the model *)
      let model = Solver.get_model solver |> Option.get in
      Printf.printf "Model: %s\n" (Model.to_string model)
  | Solver.UNSATISFIABLE ->
      Printf.printf "The constraints are unsatisfiable.\n"
  | Solver.UNKNOWN ->
      Printf.printf "The satisfiability of the constraints could not be determined.\n"

(* Take a list of verification conditions in z3 and 
   report boolean representing whether the constrint is satisfiable.
   if satisfiable returns true, 
   if unsat returns false, 
   if unknown returns false and write error to logs.
   *)
let is_sat (_constraints : Expr.expr list) : bool = 
  (* Create a Z3 configuration and context *)
  (* Create a solver and add the constraints *)
  let solver = Solver.mk_solver ctx None in
  (* Combine the constraints *)
  let combined_constraints = Boolean.mk_and ctx _constraints in
  Solver.add solver [combined_constraints];
  (* Check satisfiability *)
  match Solver.check solver [] with
  | Solver.SATISFIABLE ->
      Printf.printf "The constraints are satisfiable.\n";
      (* Print the model *)
      let model = Solver.get_model solver |> Option.get in
    Printf.printf "Model: %s\n" (Model.to_string model);
    true
  | Solver.UNSATISFIABLE ->
    Printf.printf "The constraints are unsatisfiable.\n";
    false
  | Solver.UNKNOWN ->
    Printf.printf "The satisfiability of the constraints could not be determined.\n";
    false

let _extract_refinement (_var:expression) (_predicate:expression) = 
  failwith "Not implemented"
(*
let handle_structure_item (str_item: Parsetree.structure_item) : Expr.expr = 

  match str_item.pstr_desc with
    | Pstr_value (Nonrecursive, (bindings :value_binding list)) -> 
      (
        match bindings with
        | binding::[] -> 
          (
          match binding.pvb_pat.ppat_desc with
          | Ppat_constraint (_, x) -> (
            match x with 
            | {ptyp_desc; _} -> (
              match ptyp_desc with
              | Ptyp_poly(_,x) -> (
                match x with
                |{ptyp_attributes;_} -> (
                  match ptyp_attributes with 
                  | [{attr_name;attr_payload;_}] -> (
                    match attr_name with 
                    | {txt = "refinement";_} -> (
                      match attr_payload with
                      | PStr [{pstr_desc;_}] -> (
                        match pstr_desc with 
                        | Pstr_eval ({pexp_desc;_},_) -> (
                          match pexp_desc with 
                          | Pexp_tuple [refinementVariableExpr; refinementPredicateExpr] -> extract_refinement refinementVariableExpr refinementPredicateExpr
                          |_ -> true_
                        )
                        |_ -> true_
                      )
                      |_ -> true_
                    )
                    | _ -> true_
                  )
                  |_ -> true_
                )
                (* |_ -> true_ *)
              )
              | _ -> true_
            )
            (* | _ -> true_ *)
          )
          | x -> 
            print_endline("SUMAN");
           true_
        )
          (* Extract refinement type
          | Ppat_constraint (_, {ptyp_desc = Ptyp_poly (_, {ptyp_attributes = [
            {attr_name = {txt = "refinement"; _}; 
            attr_payload = PStr [
              {pstr_desc = Pstr_eval ({pexp_desc = 
              Pexp_tuple [refinementVariableExpr; refinementPredicateExpr] ; _ }, _); _}
            ]; 
            _
            } 
          ]; _}); _}) -> 
            print_endline "FOOBAR";
            extract_refinement refinementVariableExpr refinementPredicateExpr
          | _ -> true_
          ) *)

       | _ -> failwith "Not supported"
       )
    
    | _ -> failwith "Not supported"
 *)   


let get_var_from_pattern (binding : pattern) : string = 
  match binding with 
  | {ppat_desc = Ppat_var {txt; _};_} -> txt
  | _ -> failwith "unexpected pattern"
  


let handle_structure_item (str_item: Parsetree.structure_item) : Expr.expr = 
  match str_item.pstr_desc with 
  | Pstr_value (Nonrecursive, [binding]) -> (
    (* extract var *)
    let lhs_var = get_var_from_pattern binding.pvb_pat in
    (* extract rhs *)
    (* let rhs_exp = get_rhs_from_struct_item binding.pvb_expr in *)
    (* Constraint matchin *)
    let cstrn =  binding.pvb_constraint in
    match cstrn with
    | Some(x) -> (
      match x with
      | Pvc_constraint {typ = {ptyp_attributes = [{
        attr_name = {
          txt = "refinement";
          _
        };
        attr_payload = PStr [{
          pstr_desc = Pstr_eval ({
            pexp_desc = Pexp_tuple [_refVar; _refPredicate];
            _
          }, _)
        ; _}];
         _
      }]; _}; _} -> 
        print_endline "suman";
        true_
      | _ -> failwith "Not supported" (* Some other type than simple type *)
    )
    | _ -> true_ (*No type information provided*)
  )
  |_ ->  failwith "Not supported"
     


let rec get_verificaiton_condition (ast: Parsetree.structure) (conditions: Expr.expr list) = 
  (* Go through each structure_item, and keep adding/building the verification condition *)
  match ast with 
  | [] -> []
  | head::tail -> let new_cond = handle_structure_item head in
      get_verificaiton_condition tail (new_cond::conditions)



type refinement = {
  variable: string;
  predicate: expression;
}

let int_from_constant (candidate: constant) : int = 
  match candidate with
  | Pconst_integer (x, _)  -> int_of_string(x)
  | _ -> failwith "provided constant is not an integer"


let convert_assignment_refinement_to_expr (refn: refinement) =
  let pred = refn.predicate in
  let var = refn.variable in
  let v = Expr.mk_const ctx (Symbol.mk_string ctx var) (Arithmetic.Integer.mk_sort ctx) in ()
  (* let pred_expr =
    match pred with *)
let convert_expression_to_expr (expr: expression) = 
  match expr.pexp_desc with 
  | Pexp_apply (func, args) ->
    convert_application_to_expr expr.pexp_desc
  
  | _ -> failwith "Not supported"
let rec convert_application_to_expr (desc: expression_desc) = 
  let (func, args) = desc in
  let ident = func.pexp_desc in
  match ident with
  
    | Pexp_ident op ->
      match op.txt with
      | Lident "+" -> 
        match args with
        
        | [lhs; rhs] -> Arithmetic.mk_add ctx [convert_expression_to_expr lhs; convert_expression_to_expr rhs]
        | _ -> failwith "Not supported more argumments than defined for application"
 
    
  
  | _ -> failwith "Not supported application type"

let type_check program = 
  let lexbuf = Lexing.from_string program in 
  let ast = Parse.implementation lexbuf in
  let vc = get_verificaiton_condition ast [] in
  match is_sat vc with  
    | true -> Ok ()
  (* TODO: Future work: add custom error messages *)
    | _ -> Error "failed to type check."

