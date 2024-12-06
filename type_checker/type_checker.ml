open Z3
open Parsetree
open Type_table

let cfg = [("model", "true")];;
let ctx = Z3.mk_context cfg;;

let true_ = Boolean.mk_val ctx true

type _type_constraint = {
  ttype: string;
  constraints: Expr.expr;
}

let _type_constraints : type_constraint list ref = ref [];;

(* let hashmap = Hashtbl.create 10;; *)


(* type vartype = 
  | Int of int
  | Bool of bool

 type refinement = {
  (* variable_type: vartype; *)
  variable_type: vartype;
  variable: string;
  predicate: expression;
} *)

let int_from_constant (candidate: constant) : int = 
  match candidate with
  | Pconst_integer (x, _)  -> int_of_string(x)
  | _ -> failwith "provided constant is not an integer"

  (* let pred_expr =
    match pred with *)
let rec convert_expression_to_expr (expr: expression) = 
  match expr.pexp_desc with
  | Pexp_apply (func, args) ->
    let expr = convert_application_to_expr func args in
    (* Printf.printf "Expression: %s\n" (Expr.to_string expr); *)
    expr
  | Pexp_constant (c) -> 
    let int_val = int_from_constant c in
    Arithmetic.Integer.mk_numeral_i ctx int_val
  | Pexp_ident op -> (
    match op.txt with 
    | Lident x -> 
      let var = make_z3_var ctx x in
      (* let var = Expr.mk_const ctx (Symbol.mk_string ctx x) (Arithmetic.Integer.mk_sort ctx) in *)
      var
    | _ -> failwith "Not supported Type"
    )
  | Pexp_construct ({txt = Lident str;_},_) ->  (Boolean.mk_val ctx (bool_of_string(str)))
  | _ -> failwith "Not supported Type"
and convert_application_to_expr (func: expression) (args) = 
  let ident = func.pexp_desc in
    match ident with  
      | Pexp_ident op ->  (       
        match op.txt with
        | Lident "+" -> (
          match args with
          (_, lhs_expr)::(_, rhs_expr)::[] -> (
            (* print_endline("parsing +"); *)
                let lhs_z3 = convert_expression_to_expr lhs_expr in
                let rhs_z3 = convert_expression_to_expr rhs_expr in
                Arithmetic.mk_add ctx [lhs_z3; rhs_z3] 
              ) 
          |_ -> failwith "Incorrectly Typed +"        
        )
        | Lident "-" -> (
          match args with
          (_, lhs_expr)::(_, rhs_expr)::[] -> (
            (* print_endline("parsing -"); *)
                let lhs_z3 = convert_expression_to_expr lhs_expr in
                let rhs_z3 = convert_expression_to_expr rhs_expr in
                Arithmetic.mk_sub ctx [lhs_z3; rhs_z3] 
              ) 
          |_ -> failwith "Incorrectly Typed -"        
        )
        | Lident "*" -> (
          match args with
          (_, lhs_expr)::(_, rhs_expr)::[] -> (
            (* print_endline("parsing *"); *)
                let lhs_z3 = convert_expression_to_expr lhs_expr in
                let rhs_z3 = convert_expression_to_expr rhs_expr in
                Arithmetic.mk_mul ctx [lhs_z3; rhs_z3] 
              ) 
          |_ -> failwith "Incorrectly Typed *"        
        )     
        | Lident "/" -> (
          match args with
          (_, lhs_expr)::(_, rhs_expr)::[] -> (
            (* print_endline("parsing /"); *)
                let lhs_z3 = convert_expression_to_expr lhs_expr in
                let rhs_z3 = convert_expression_to_expr rhs_expr in
                Arithmetic.mk_div ctx lhs_z3 rhs_z3
              ) 
          |_ -> failwith "Incorrectly Typed /"        
        )   
        | Lident "&&" -> (
          match args with
          (_, lhs_expr)::(_, rhs_expr)::[] -> (
            (* print_endline("parsing &&"); *)
                let lhs_z3 = convert_expression_to_expr lhs_expr in
                let rhs_z3 = convert_expression_to_expr rhs_expr in
                Boolean.mk_and ctx [lhs_z3; rhs_z3]
              ) 
          |_ -> failwith "Incorrectly Typed &&"        
        )  
        | Lident "||" -> (
          match args with
          (_, lhs_expr)::(_, rhs_expr)::[] -> (
            (* print_endline("parsing ||"); *)
                let lhs_z3 = convert_expression_to_expr lhs_expr in
                let rhs_z3 = convert_expression_to_expr rhs_expr in
                Boolean.mk_or ctx [lhs_z3; rhs_z3]
              ) 
          |_ -> failwith "Incorrectly Typed ||"        
        )
        | Lident "=" -> (
          match args with
          (_, lhs_expr)::(_, rhs_expr)::[] -> (
            (* print_endline("parsing ="); *)
                let lhs_z3 = convert_expression_to_expr lhs_expr in
                let rhs_z3 = convert_expression_to_expr rhs_expr in
                Boolean.mk_eq ctx lhs_z3 rhs_z3
              ) 
          |_ -> failwith "Incorrectly Typed ="        
        )           
        | Lident ">" -> (
          match args with
          (_, lhs_expr)::(_, rhs_expr)::[] -> (
            (* print_endline("parsing >");           *)
                let lhs_z3 = convert_expression_to_expr lhs_expr in
                let rhs_z3 = convert_expression_to_expr rhs_expr in
                Arithmetic.mk_gt ctx lhs_z3 rhs_z3
            )
          |_ -> failwith "Incorrectly Typed  >"        
        )
        | Lident "<" -> (
          match args with
          (_, lhs_expr)::(_, rhs_expr)::[] -> (
            (* print_endline("parsing <"); *)
          
                let lhs_z3 = convert_expression_to_expr lhs_expr in
                let rhs_z3 = convert_expression_to_expr rhs_expr in
                let temp = Arithmetic.mk_lt ctx lhs_z3 rhs_z3 in 
                (* Printf.printf "Expression: %s\n" (Expr.to_string temp); *)
                temp
            )
          |_ -> failwith "Incorrectly Typed <"        
        )
        | Lident "<=" -> (
          match args with
          (_, lhs_expr)::(_, rhs_expr)::[] -> (
            (* print_endline("parsing <="); *)
          
                let lhs_z3 = convert_expression_to_expr lhs_expr in
                let rhs_z3 = convert_expression_to_expr rhs_expr in
                let temp = Boolean.mk_or ctx [Arithmetic.mk_lt ctx lhs_z3 rhs_z3; Boolean.mk_eq ctx lhs_z3 rhs_z3] in 
                (* Printf.printf "Expression: %s\n" (Expr.to_string temp); *)
                temp
            )
          |_ -> failwith "Incorrectly Typed <="   
        )
        | Lident ">=" -> (
          match args with
          (_, lhs_expr)::(_, rhs_expr)::[] -> (
            (* print_endline("parsing <=");           *)
                let lhs_z3 = convert_expression_to_expr lhs_expr in
                let rhs_z3 = convert_expression_to_expr rhs_expr in
                let temp = Boolean.mk_or ctx [Arithmetic.mk_gt ctx lhs_z3 rhs_z3; Boolean.mk_eq ctx lhs_z3 rhs_z3] in 
                (* Printf.printf "Expression: %s\n" (Expr.to_string temp); *)
                temp
            )
          |_ -> failwith "Incorrectly Typed <="        
        )
        | _ -> failwith "Not supported Operation"
      )
      |_ -> failwith "Invalid Refinement Predicate Syntax"


(* let _convert_assignment_refinement_to_expr (refn: refinement) =
  let pred = refn.predicate in
  let var = refn.variable in
  let _var_t = refn.variable_type in
  let _v = Expr.mk_const ctx (Symbol.mk_string ctx var) (Arithmetic.Integer.mk_sort ctx) in 
  let _pred_expr = (convert_expression_to_expr pred) in () *)

  
(* Take a list of verification conditions in z3 and 
   report boolean representing whether the constrint is satisfiable.
   if satisfiable returns true, 
   if unsat returns false, 
   if unknown returns false and write error to logs.
   *)
let is_sat (_constraints : Expr.expr list) : bool = 
  (* Create a Z3 configuration and context *)
  (* Create a solver and add the constraints *)

  let expr_strings = List.map Expr.to_string _constraints in
  List.iter (Printf.printf "Expression: %s\n") expr_strings;

  let solver = Solver.mk_solver ctx None in
  (* Combine the constraints *)
  let combined_constraints = Boolean.mk_and ctx _constraints in
  Solver.add solver [combined_constraints];

  (* Check satisfiability *)
  match Solver.check solver [] with
  | Solver.SATISFIABLE ->
      Printf.printf "The constraints are satisfiable.\n";
      (* Print the model *)
      (* let model = Solver.get_model solver |> Option.get in
    Printf.printf "Model: %s\n" (Model.to_string model); *)
    true
  | Solver.UNSATISFIABLE ->
    Printf.printf "The constraints are unsatisfiable.\n";
    false
  | Solver.UNKNOWN ->
    Printf.printf "The satisfiability of the constraints could not be determined.\n";
    false
 
let get_var_from_pattern (binding : pattern) : string = 
  match binding with 
  | {ppat_desc = Ppat_var {txt; _};_} -> txt
  | _ -> failwith "unexpected pattern"

(* 
let get_var_from_exp (expr : expression) : string = 
  match expr with 
  | {pexp_desc = Pexp_ident { txt = Lident x; _ };_}  -> x
  |_ -> failwith "unexpected pattern" *)


let _create_z3_variable (name: string) (kind: ttypes) = 
    match kind with
    | Tint -> Expr.mk_const ctx (Symbol.mk_string ctx name) (Arithmetic.Integer.mk_sort ctx)
    | Tbool -> Expr.mk_const ctx (Symbol.mk_string ctx name) (Boolean.mk_sort ctx)
    (* | _ -> failwith ("variable of type" ^  (ttype_string  kind) ^ " is not implemented") *)

(* let convert_type_constraints (expr: type_declaration) = () *)


let handle_structure_item (str_item: Parsetree.structure_item) : Expr.expr list = 
  match str_item.pstr_desc with 
  | Pstr_value (Nonrecursive, [binding]) -> (
    let lhs_var = get_var_from_pattern binding.pvb_pat in (* LHS variable *)
    let rhs_exp = binding.pvb_expr in
    (* The following is trying to extract the refinement type  *)
    let cstrn =  binding.pvb_constraint in
    match cstrn with
    | Some(x) -> (
      match x with
      | Pvc_constraint {typ = {
        ptyp_desc = Ptyp_constr ({txt = Lident base_type; _}, _);
        ptyp_attributes = [{
        attr_name = {
          txt = "refinement";
          _
        };
        attr_payload = PStr [{
          pstr_desc = Pstr_eval ({
            pexp_desc = Pexp_tuple [
              
              {pexp_desc = Pexp_ident { txt = Lident auxilary_var; _ };_}; 
              
              refPredicate
              
              ];
            _
          }, _)
        ; _}];
         _
      }]; _}; _} -> 
        (*
          Here, we have access to: 
          LHS variable: lhs_var
          RHS expression: rhs_exp
          Refinement type: {base_type, auxilary_var, predicate}
          TODO:
          Based on these things we need to generate a Z3 constraint.
          Base type might be needed as it will help you know what kind of z3 variable to create
        *)
        let bttype = string_ttype base_type in
        let _ = add_type lhs_var bttype in
        let _ = add_type auxilary_var bttype in
        let lhs_z3_var = make_z3_var ctx lhs_var in
        let refinement_aux_var = make_z3_var ctx auxilary_var in
        let rhs_constraints =  Boolean.mk_eq ctx lhs_z3_var (convert_expression_to_expr rhs_exp) in
        let ref_var_eq_lhs_var_constraint = Boolean.mk_eq ctx lhs_z3_var refinement_aux_var in
        let refinement_predicate_contraint = convert_expression_to_expr refPredicate in 
        [refinement_predicate_contraint; rhs_constraints; ref_var_eq_lhs_var_constraint]

        (* 
        (* Debuggin info: *)
        print_string "base_type "; 
        print_endline base_type;
        print_string "var ";
        print_string "auxilary_var "; 
        print_endline auxilary_var; 
        *)
      | Pvc_constraint {typ = {
        ptyp_desc = Ptyp_constr ({txt = Lident base_type; _}, _);
        
        ptyp_attributes = []; _}; _} -> 
          let bttype = string_ttype base_type in
          let _ = add_type lhs_var bttype in
          let lhs_z3_var = make_z3_var ctx lhs_var in
          let rhs_constraints =  Boolean.mk_eq ctx lhs_z3_var (convert_expression_to_expr rhs_exp) in
          (* print_endline("DID match"); *)
          [rhs_constraints]


      | _ -> failwith "Not supported" (* Some other type than simple type *)
    )
    | _ -> [true_] (*No type information provided*)
  )
  (* | Pstr_type (Recursive, type_decl_list) -> 
    let c_constraint = ref [] in
      List.iter (fun x -> c_constraint := x :: !c_constraint) 
      (List.map convert_type_constraints type_decl_list);
    [true_]; *)

  |_ ->  failwith "Not supported"



let rec get_verificaiton_condition (ast: Parsetree.structure) (conditions: Expr.expr list) = 
  (* Go through each structure_item, and keep adding/building the verification condition *)
  match ast with 
  | [] -> conditions
  | head::tail -> let new_conds = handle_structure_item head in
      (* print_endline("added new conds"); *)
      get_verificaiton_condition tail (new_conds@conditions)


let type_check program = 
  let lexbuf = Lexing.from_string program in 
  let ast = Parse.implementation lexbuf in
  let vc = get_verificaiton_condition ast [] in
  match is_sat vc with  
    | true -> Ok ()
    | _ -> print_endline("Failed to Type Check");
    Error "Failed to Type Check"

