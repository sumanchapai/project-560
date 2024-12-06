
type ttypes = 
| Tbool 
| Tint
let hashmap = Hashtbl.create 100;;


let exists_type (name: string) = Hashtbl.mem hashmap name;;
let add_type (name: string) (t: ttypes) = 
  Hashtbl.add hashmap name t;;

let get_type (name: string) = Hashtbl.find hashmap name;;



let ttype_string (t: ttypes) = match t with
| Tbool -> "bool"
| Tint -> "int";;

let string_ttype (s: string) = match s with
| "bool" -> Tbool
| "int" -> Tint
| _ -> failwith "Invalid type";;

let make_z3_var ctx name = 
  let t = get_type name in
  match t with
  | Tbool -> Z3.Boolean.mk_const_s ctx name
  | Tint -> Z3.Arithmetic.Integer.mk_const_s ctx name;;
