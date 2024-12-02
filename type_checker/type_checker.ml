(* This library is supposed to take an OCaml programs with refinement types 
   added to it in the form of attributes, and report whether or not the program
   type checks. If the program doesn't type check, then it returns the errors
   in the program.
   *)

let type_check _program = 
  let custom_failure : (unit, string) result = 
    Error "Type checker isn't implemented: line:1 column:1"
  in custom_failure
