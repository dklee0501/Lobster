open Circuit
open Subregion

let rec print_varlist : var list -> unit = fun varlist ->
  match varlist with
  | [] -> print_newline()
  | var::tl -> let _ = print_string(var ^ " ") in print_varlist tl

let rec varlist_to_string : var list -> string = fun varlist ->
  match varlist with
  | [] -> "\n"
  | var::tl -> var ^ " " ^ varlist_to_string tl

let rec bexp_to_string : bexp -> string = fun bexp ->
  match bexp with
  | CONST b -> if(b) then "true" else "false" 
  | VAR v -> v
  | AND (b1, b2) -> "(and " ^ bexp_to_string b1 ^ " " ^ bexp_to_string b2 ^ ")"
  | XOR (b1, b2) -> "(xor " ^ bexp_to_string b1 ^ " " ^ bexp_to_string b2 ^ ")"
  | OR (b1, b2) -> "(or " ^ bexp_to_string b1 ^ " " ^ bexp_to_string b2 ^ ")"
  | NOT b1 -> "(not " ^ bexp_to_string b1 ^ ")"
  | NULL -> raise (Error "NULL var is used")

let rec print_bexp : bexp -> unit = fun bexp ->
  match bexp with
  | CONST b -> if(b) then print_string "true" else print_string "false" 
  | VAR v -> print_string v
  | AND (b1, b2) -> print_string("(and "); print_bexp b1; print_string(" "); print_bexp b2; print_string(")")
  | XOR (b1, b2) -> print_string("(xor "); print_bexp b1; print_string(" "); print_bexp b2; print_string(")")
  | OR (b1, b2) -> print_string("(or "); print_bexp b1; print_string(" "); print_bexp b2; print_string(")")
  | NOT b -> print_string("(not "); print_bexp b; print_string(")")
  | NULL -> raise (Error "NULL var is used")


let print_eqn : eqn -> unit = fun eqn ->
  let (lexp, bexp) = eqn in
  let _ = print_string(lexp ^ " = ") in
  let _ = print_bexp bexp in
  print_newline() 

let rec print_eqnlist : eqn list -> unit = fun eqnlist ->
  match eqnlist with
  | [] -> print_newline()
  | eqn::tl -> print_eqn(eqn);print_eqnlist(tl)

let eqn_to_string : eqn -> string = fun eqn ->
  let (lexp, bexp) = eqn in
  let str1 = lexp ^ " = " in
  let str2 = bexp_to_string bexp in
  str1 ^ str2 ^ "\n"

let rec elist_to_string : eqn list -> string = fun elist ->
  match elist with
  | [] -> "\n"
  | e::tl -> (eqn_to_string e) ^ (elist_to_string tl)

let print_circuit : circuit -> unit = fun circuit ->
  let (inlist, outlist, eqnlist) = circuit in
  let _ = print_string("Input list\n"); print_varlist inlist in
  let _ = print_string("Output list\n"); print_varlist outlist in
  print_eqnlist eqnlist

let circuit_to_string_expanded : circuit -> string = fun cir ->
  let (ilist, olist, elist) = cir in
  let input_string = "Input List\n" ^ varlist_to_string ilist in
  let output_string = "Output List\n" ^ varlist_to_string olist in
  let expanded_elist = List.map (fun o -> (o, expand cir (VAR o))) olist in
  let eqn_string = elist_to_string expanded_elist in
  input_string ^ output_string ^ eqn_string ^ "\n"

let printf_expanded_circuit : string -> circuit -> unit = fun fname -> fun cir ->
  let file = open_out fname in
  let _ = output_string file (circuit_to_string_expanded cir) in
  let _ = close_out file in
  ()  

let print_subregion : circuit -> subregion -> unit = fun cir -> fun sub ->
  let vdmap = get_var_depth_map cir in
  let (ilist, olist, elist) = sub in
  let ilist_with_depth = List.map (fun x -> (x ^ "(d" ^ string_of_int(find_depth vdmap x) ^ ")"  )) ilist in
  let olist_with_depth = List.map (fun x -> (x ^ "(d" ^ string_of_int(find_depth vdmap x) ^ ")"  )) olist in
  let _ = print_string("Input list\n"); print_varlist ilist_with_depth in
  let _ = print_string("Output list\n"); print_varlist olist_with_depth in
  print_eqnlist elist


let print_var_depth_map : circuit -> unit = fun cir ->
  let vdmap = get_var_depth_map cir in
  let rec print_vdmap :depth_map -> unit = fun vdmap ->
    match vdmap with
    | [] -> print_newline();
    | (v,d)::tl -> print_endline("Var : " ^ v ^ " Depth : " ^ (string_of_int d));print_vdmap tl
  in
  print_vdmap vdmap




