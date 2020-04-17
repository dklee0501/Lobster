open Graph_circuit
open Unify
open Circuit

let norm_var_index = ref 0

let norm_new_var : unit -> string = fun unit -> 
  let new_index = !norm_var_index in 
  let _ = norm_var_index := !norm_var_index + 1 in
  "norm" ^ (string_of_int new_index)


let rec substitute_bexp : (bexp * var) -> bexp -> bexp = fun (old, normvar) -> fun bexp ->
  let rec subst_once : bexp -> bexp = fun bexp ->
    if(is_equal_bexp bexp old) then 
      VAR normvar 
    else
      match bexp with
      | NULL -> raise (Error "null")
      | CONST _  -> bexp
      | VAR x -> bexp
      | AND (b1, b2) -> 
        let subst_b1 = subst_once b1 in
        let subst_b2 = subst_once b2 in
        let b1b2 = AND (subst_b1, subst_b2) in
        if(is_equal_bexp b1b2 old) then VAR normvar else b1b2
      | XOR (b1, b2) -> 
        let subst_b1 = subst_once b1 in
        let subst_b2 = subst_once b2 in
        let b1b2 = XOR (subst_b1, subst_b2) in
        if(is_equal_bexp b1b2 old) then VAR normvar else b1b2
      | OR (b1, b2) -> 
        let subst_b1 = subst_once b1 in
        let subst_b2 = subst_once b2 in
        let b1b2 = OR (subst_b1, subst_b2) in
        if(is_equal_bexp b1b2 old) then VAR normvar else b1b2
      | NOT (b1) -> 
        let subst_b1 = subst_once b1 in
        let nb1 = NOT (subst_b1) in
        if(is_equal_bexp nb1 old) then VAR normvar else nb1
  in
  let subst_bexp = subst_once bexp in
  if(subst_bexp = bexp) then bexp else substitute_bexp (old, normvar) subst_bexp
 


let is_subterm : bexp -> bexp -> var -> bool = fun bexp -> fun oldb -> fun var ->
  let old_vlist = get_bexp_varlist oldb in
  let sub_old = substitute_bexp (oldb, var) bexp in
  let sub_old_vlist = get_bexp_varlist sub_old in
  let subterm_check = List.filter (fun x -> List.mem x old_vlist) sub_old_vlist in
  (subterm_check = []) && (not ((get_bexp_size sub_old) = (get_bexp_size bexp)))
 

let rec get_subterms : bexp -> bexp list = fun bexp ->
  match bexp with
  | NULL -> raise (Error "null var")
  | CONST _ 
  | VAR _ -> []
  | AND(b1, b2)
  | XOR(b1, b2)
  | OR(b1, b2) -> 
    let b1_subterms = get_subterms b1 in
    let b2_subterms = get_subterms b2 in
    bexp::(b1_subterms @ b2_subterms)
  | NOT b1 -> bexp::(get_subterms b1)
  

let normalize_case_once : (bexp * bexp) -> (bexp * bexp) = fun (oldb, newb) ->
  let old_subterms = get_subterms oldb in
  let rec helper : bexp list -> (bexp * bexp) = fun old_subterms ->
    match old_subterms with
    | [] -> (oldb, newb)
    | hd::tl ->
      let hd_newvar = norm_new_var() in
      if(is_subterm newb hd hd_newvar) then
        let sub_old = substitute_bexp (hd, hd_newvar) oldb in
        let sub_new = substitute_bexp (hd, hd_newvar) newb in
        (sub_old, sub_new)
      else
        helper tl
  in
  helper old_subterms
  

let rec normalize_case_helper : (bexp * bexp) -> (bexp * bexp) = fun (oldb, newb) ->
  let normalize_once = normalize_case_once(oldb, newb) in
  if((oldb, newb) = normalize_once) then 
    normalize_once 
  else 
    normalize_case_helper normalize_once

let normalize_case : (bexp * bexp) -> (bexp * bexp) = fun (oldb, newb) ->
  let _ = norm_var_index := 0 in  
  normalize_case_helper (oldb, newb)