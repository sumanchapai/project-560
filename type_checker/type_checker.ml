open Z3
(* This library is supposed to take an OCaml programs with refinement types 
   added to it in the form of attributes, and report whether or not the program
   type checks. If the program doesn't type check, then it returns the errors
   in the program.
   *)

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
  let cfg = [("model", "true")] in
  let ctx = Z3.mk_context cfg in
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


(* TODO: Generate program verification condition from the program AST *)
let get_verificaiton_condition _ast = []


let type_check program = 
  let lexbuf = Lexing.from_string program in 
  let ast = Parse.implementation lexbuf in
  let vc = get_verificaiton_condition ast in
  match is_sat vc with  
    | true -> Ok ()
  (* TODO: Add custom error messages *)
    | _ -> Error "failed to type check."
