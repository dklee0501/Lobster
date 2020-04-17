open Circuit
open Graph_circuit

type cbexp = CONST of bool 
	   | CVAR of string
	   | VAR of string
	   | AND of (cbexp * cbexp) 
	   | XOR of (cbexp * cbexp) 
	   | OR of (cbexp * cbexp) 
	   | NOT of cbexp
type subst = (var * cbexp) list




let rec print_cbexp : cbexp -> unit = fun cbexp ->
  match cbexp with
  | CONST b -> if(b) then print_string "true" else print_string "false"
  | CVAR v -> print_string ("CVar_" ^ v)
  | VAR v -> print_string v
  | AND (b1, b2) -> print_string("(and "); print_cbexp b1; print_string(" "); print_cbexp b2; print_string(")")
  | XOR (b1, b2) -> print_string("(xor "); print_cbexp b1; print_string(" "); print_cbexp b2; print_string(")")
  | OR (b1, b2) -> print_string("(or "); print_cbexp b1; print_string(" "); print_cbexp b2; print_string(")")
  | NOT b -> print_string("(not "); print_cbexp b; print_string(")")



let print_subst : subst -> unit = fun subst ->
  let _ = print_endline("----print_subst start----") in
  let _ = List.iter (fun (old_var, new_cbexp) -> (print_string old_var); print_string("  -->  "); print_cbexp new_cbexp; print_newline()) subst in
  print_endline("---------------------")


let rec bexp2cbexp : bexp -> cbexp = fun bexp -> 
  match bexp with
  | NULL -> raise (Error "null is used")
  | CONST b -> CONST b
  | VAR x -> CVAR x
  | AND (b1, b2) -> 
    let cbexp_b1 = bexp2cbexp b1 in
    let cbexp_b2 = bexp2cbexp b2 in
    AND (cbexp_b1, cbexp_b2)
  | XOR (b1, b2) -> 
    let cbexp_b1 = bexp2cbexp b1 in
    let cbexp_b2 = bexp2cbexp b2 in
    XOR (cbexp_b1, cbexp_b2)
  | OR (b1, b2) -> 
    let cbexp_b1 = bexp2cbexp b1 in
    let cbexp_b2 = bexp2cbexp b2 in
    OR (cbexp_b1, cbexp_b2)
  | NOT b1 -> 
    let cbexp_b1 = bexp2cbexp b1 in
    NOT cbexp_b1

let rec bexp2cbexp_onlytype : bexp -> cbexp = fun bexp -> 
  match bexp with
  | NULL -> raise (Error "null is used")
  | CONST b -> CONST b
  | VAR x -> VAR x
  | AND (b1, b2) -> 
    let cbexp_b1 = bexp2cbexp_onlytype b1 in
    let cbexp_b2 = bexp2cbexp_onlytype b2 in
    AND (cbexp_b1, cbexp_b2)
  | XOR (b1, b2) -> 
    let cbexp_b1 = bexp2cbexp_onlytype b1 in
    let cbexp_b2 = bexp2cbexp_onlytype b2 in
    XOR (cbexp_b1, cbexp_b2)
  | OR (b1, b2) -> 
    let cbexp_b1 = bexp2cbexp_onlytype b1 in
    let cbexp_b2 = bexp2cbexp_onlytype b2 in
    OR (cbexp_b1, cbexp_b2)
  | NOT b1 -> 
    let cbexp_b1 = bexp2cbexp_onlytype b1 in
    NOT cbexp_b1


let rec cbexp2bexp : cbexp -> bexp = fun cbexp -> 
  match cbexp with
  | CONST b -> CONST b
  | CVAR x -> VAR x
  | VAR x -> VAR x
  | AND (b1, b2) -> 
    let bexp_b1 = cbexp2bexp b1 in
    let bexp_b2 = cbexp2bexp b2 in
    AND (bexp_b1, bexp_b2)
  | XOR (b1, b2) -> 
    let bexp_b1 = cbexp2bexp b1 in
    let bexp_b2 = cbexp2bexp b2 in
    XOR (bexp_b1, bexp_b2)
  | OR (b1, b2) -> 
    let bexp_b1 = cbexp2bexp b1 in
    let bexp_b2 = cbexp2bexp b2 in
    OR (bexp_b1, bexp_b2)
  | NOT b1 -> 
    let bexp_b1 = cbexp2bexp b1 in
    NOT bexp_b1


let rec contain_const : cbexp -> bool = fun cbexp ->
  match cbexp with
  | CONST _ | CVAR _ -> true
  | VAR _ -> false
  | AND (b1, b2) | XOR (b1, b2) | OR (b1, b2) -> (contain_const b1) || (contain_const b2)
  | NOT b -> contain_const b


let rec alpha_subst : (var, var) BatMap.t -> bexp -> cbexp = fun subst -> fun bexp ->
  match bexp with
  | NULL -> raise (Error "null is used")
  | CONST b -> CONST b
  | VAR x -> if (BatMap.mem x subst) then VAR (BatMap.find x subst) else VAR x 
  | AND (b1, b2) -> 
    let alpha_b1 = alpha_subst subst b1 in
    let alpha_b2 = alpha_subst subst b2 in
    AND (alpha_b1, alpha_b2)
  | OR (b1, b2) -> 
    let alpha_b1 = alpha_subst subst b1 in
    let alpha_b2 = alpha_subst subst b2 in
    OR (alpha_b1, alpha_b2)
  | XOR (b1, b2) -> 
    let alpha_b1 = alpha_subst subst b1 in
    let alpha_b2 = alpha_subst subst b2 in
    XOR (alpha_b1, alpha_b2)
  | NOT b1 -> 
    let alpha_b1 = alpha_subst subst b1 in
    NOT alpha_b1


let rec alpha_conv : (bexp * bexp) -> (cbexp * cbexp) = fun (old_bexp, new_bexp) ->
  let old_vlist = get_bexp_varlist old_bexp in
  let new_vlist = get_bexp_varlist new_bexp in
  (*let tgt_vlist = get_bexp_varlist tgt_bexp in
  let vlist_to_conv = List.filter (fun x -> List.mem x tgt_vlist) (list_merge new_vlist old_vlist) in*)
  let vlist_to_conv = list_merge new_vlist old_vlist in 
  let subst_list = List.mapi (fun i -> fun x -> (x, "tmpvar#"^(string_of_int i))) vlist_to_conv in
  let subst = List.fold_left (fun map -> fun (old_v, tmp_v) -> BatMap.add (old_v) (tmp_v) map) BatMap.empty subst_list in
  (alpha_subst subst old_bexp, alpha_subst subst new_bexp)  


let rec is_equal_bexp : bexp -> bexp -> bool = fun b1 -> fun b2 ->
  match (b1, b2) with
  | (NULL, _) | (_, NULL) -> raise (Error "null used")
  | (CONST b1, CONST b2) -> (b1 = b2)
  | (VAR v1, VAR v2) -> (v1 = v2)
  | (AND (a, b), AND(c, d)) 
  | (XOR (a, b), XOR(c, d)) 
  | (OR (a, b), OR(c, d)) -> (is_equal_bexp a c) && (is_equal_bexp b d) 
  | (NOT b1, NOT b2) -> is_equal_bexp b1 b2
  | _ -> false

let rec is_equal_cbexp : cbexp -> cbexp -> bool = fun b1 -> fun b2 ->
  match (b1, b2) with
  | (CONST b1, CONST b2) -> (b1 = b2)
  | (CVAR x1, CVAR x2) -> x1 = x2
  | (VAR v1, VAR v2) -> (v1 = v2)
  | (AND (a, b), AND(c, d)) 
  | (XOR (a, b), XOR(c, d)) 
  | (OR (a, b), OR(c, d)) -> ((is_equal_cbexp a c) && (is_equal_cbexp b d)) 
  | (NOT b1, NOT b2) -> is_equal_cbexp b1 b2
  | _ -> false



(*
let rec substitute_cbexp : subst -> bexp -> bexp = fun subst -> fun bexp ->
  let is_whole = match subst with Bi _ -> false | _ -> true in
  if(is_whole) then
    let (from_b, to_b) = match subst with Whole s -> s | _ -> raise (Error "not whole subst") in
    if(is_equal_bexp from_b bexp) then to_b else bexp
  else  
    let s1 = match subst with Bi(s1, _) -> s1 | _ -> raise (Error "not bi subst") in
    let s2 = match subst with Bi(_, s2) -> s2 | _ -> raise (Error "not bi subst") in
    match bexp with
    | NULL -> raise (Error "null is used")
    | CONST _ | VAR _ -> bexp
    | AND (b1, b2) -> 
        let subst_b1 = substitute_cbexp s1 b1 in
        let subst_b2 = substitute_cbexp s2 b2 in
        let b1b2 = AND (subst_b1, subst_b2) in
        b1b2
    | OR (b1, b2) -> 
        let subst_b1 = substitute_cbexp s1 b1 in
        let subst_b2 = substitute_cbexp s2 b2 in
        let b1b2 = OR (subst_b1, subst_b2) in
        b1b2
     | XOR (b1, b2) -> 
        let subst_b1 = substitute_cbexp s1 b1 in
        let subst_b2 = substitute_cbexp s2 b2 in
        let b1b2 = XOR (subst_b1, subst_b2) in
        b1b2
     | NOT b1 -> 
        let subst_b1 = substitute_cbexp s1 b1 in
        let nb1 = NOT subst_b1 in
        nb1
  *) 


(*
let rec substitute_cbexp : (var, bexp) BatMap.t -> bexp -> bexp = fun subst -> fun bexp ->
  match bexp with
  | NULL -> raise (Error "null is used")
  | CONST _ -> bexp
  | VAR x -> if (BatMap.mem x subst) then (BatMap.find x subst) else bexp 
  | AND (b1, b2) -> 
    let alpha_b1 = substitute_cbexp subst b1 in
    let alpha_b2 = substitute_cbexp subst b2 in
    AND (alpha_b1, alpha_b2)
  | OR (b1, b2) -> 
    let alpha_b1 = substitute_cbexp subst b1 in
    let alpha_b2 = substitute_cbexp subst b2 in
    OR (alpha_b1, alpha_b2)
  | XOR (b1, b2) -> 
    let alpha_b1 = substitute_cbexp subst b1 in
    let alpha_b2 = substitute_cbexp subst b2 in
    XOR (alpha_b1, alpha_b2)
  | NOT b1 -> 
    let alpha_b1 = substitute_cbexp subst b1 in
    NOT alpha_b1
*)

let rec mem_subst : var -> subst -> bool = fun key -> fun subst ->
  match subst with
  | [] -> false 
  | (v, cb)::tl -> if(v = key) then true else mem_subst key tl

let rec find_subst : var -> subst -> cbexp = fun key -> fun subst ->
  match subst with
  | [] -> raise (Error "Key Not Found")
  | (v, cb)::tl -> if(v = key) then cb else find_subst key tl


(*
let rec substitute_cbexp : subst -> cbexp -> cbexp = fun subst -> fun cbexp ->
  (*let select_index = let _ = Random.self_init() in Random.int 500000 in
  let _ = if(select_index < 2) then ((print_subst subst);(Pp.print_bexp bexp);print_newline()) else () in*)
  let rec subst_once : cbexp -> cbexp = fun cbexp ->
      match cbexp with
      | CONST _ | CVAR _ -> cbexp
      | VAR x -> if (mem_subst x subst) then find_subst x subst else cbexp
      | AND (b1, b2) -> 
        let subst_b1 = subst_once b1 in
        let subst_b2 = subst_once b2 in
        let b1b2 = AND (subst_b1, subst_b2) in
        b1b2
      | OR (b1, b2) -> 
        let subst_b1 = subst_once b1 in
        let subst_b2 = subst_once b2 in
        let b1b2 = OR (subst_b1, subst_b2) in
        b1b2
      | XOR (b1, b2) -> 
        let subst_b1 = subst_once b1 in
        let subst_b2 = subst_once b2 in
        let b1b2 = XOR (subst_b1, subst_b2) in
        b1b2
      | NOT b1 -> 
        let subst_b1 = subst_once b1 in
        let nb1 = NOT subst_b1 in
        nb1
  in
  let subst_cbexp = subst_once cbexp in
  if(subst_cbexp = cbexp) then cbexp else substitute_cbexp subst subst_cbexp
 *)

let rec substitute_cbexp : subst -> cbexp -> cbexp = fun subst -> fun cbexp ->
  match cbexp with
  | CONST _ | CVAR _ -> cbexp
  | VAR x -> if (mem_subst x subst) then find_subst x subst else cbexp
  | AND (b1, b2) -> 
    let subst_b1 = substitute_cbexp subst b1 in
    let subst_b2 = substitute_cbexp subst b2 in
    let b1b2 = AND (subst_b1, subst_b2) in
    b1b2
  | XOR (b1, b2) -> 
    let subst_b1 = substitute_cbexp subst b1 in
    let subst_b2 = substitute_cbexp subst b2 in
    let b1b2 = XOR (subst_b1, subst_b2) in
    b1b2
  | OR (b1, b2) -> 
    let subst_b1 = substitute_cbexp subst b1 in
    let subst_b2 = substitute_cbexp subst b2 in
    let b1b2 = OR (subst_b1, subst_b2) in
    b1b2
  | NOT b1 -> 
    let subst_b1 = substitute_cbexp subst b1 in
    let nb1 = NOT subst_b1 in
    nb1
  
(*  let vlist = get_bexp_varlist bexp in
  List.fold_left (fun acc -> fun v -> subst_once acc) bexp vlist
*)



let unify : G.t -> cbexp -> G.vertex -> (subst * bool * bexp) = fun graph -> fun old -> fun tgt_node ->
  let empty_subst = [] in
  let false_bexp = Circuit.VAR "unify_fail" in
  let rec unify_helper : (subst * bool) -> cbexp -> G.vertex -> (subst * bool * bexp) = fun (subst, current_state) -> fun old -> fun tgt_node ->
    if(not current_state) then
      (empty_subst, false, false_bexp)
    else
      match (old, (fst tgt_node)) with
      | (CONST b1, Node.CONST b2)  -> if(b1 = b2) then (subst, true, CONST b1) else (empty_subst, false, false_bexp)
      | (CONST _, _) -> (empty_subst, false, false_bexp)
      | (CVAR x1, Node.VAR x2) -> if(x1 = x2) then (subst, true, VAR x2) else (empty_subst, false, false_bexp)
      | (CVAR _, _) -> (empty_subst, false, false_bexp)
      | (VAR x1, Node.VAR x2) -> ((x1, CVAR x2)::subst, true, VAR x2)
      | (VAR x, _) -> (empty_subst, false, false_bexp)
      | (AND (b1, b2), Node.AND)  
      | (OR (b1, b2), Node.OR)  
      | (XOR (b1, b2), Node.XOR) -> 
        let pred = G.pred graph tgt_node in
        let tgt_b1 = List.nth pred 0 in
        let tgt_b2 = List.nth pred 1 in
        let (subst_with_b1, state_b1, tgt_bexp1) = unify_helper (subst, current_state) b1 tgt_b1 in
        let (subst_with_b1_b2, state_b1_b2, tgt_bexp2) = unify_helper (subst_with_b1, state_b1) (substitute_cbexp subst_with_b1 b2) tgt_b2 in
        let tgt_bexp = 
	        match old with 
	        | AND(b1, b2) -> Circuit.AND(tgt_bexp1, tgt_bexp2)
	        | OR(b1, b2) -> Circuit.OR(tgt_bexp1, tgt_bexp2)
	        | XOR(b1, b2) -> Circuit.XOR(tgt_bexp1, tgt_bexp2)
          | _ -> raise (Error "match error")
        in
        (subst_with_b1_b2, state_b1_b2, tgt_bexp)
      | (NOT b1, Node.NOT) -> 
        let pred = G.pred graph tgt_node in
        let tgt_b1 = List.nth pred 0 in
        let (subst_with_b1, state_b1, tgt_bexp1) = unify_helper (subst, current_state) b1 tgt_b1 in
        (subst_with_b1, state_b1, (NOT tgt_bexp1))
      | (_, Node.VAR x) -> 
        if (snd tgt_node = 0) then
	        (empty_subst, false, false_bexp)
        else
          let pred = G.pred graph tgt_node in
          let _ = if(List.length pred = 1) then () else raise (Error ("unify error : invalid var " ^ x)) in
          let pred_node = List.nth pred 0 in
	        unify_helper (subst, current_state) old pred_node
      | (_, Node.CONST b) -> (empty_subst, false, false_bexp)
      | _ -> (* different op case *) (empty_subst, false, false_bexp)
	in
  let (result_subst, success, tgt_bexp) = unify_helper (empty_subst, true) old tgt_node in
  (List.rev result_subst, success, tgt_bexp)
  

let unify_cbexp : cbexp -> cbexp -> (subst * bool) = fun old -> fun tgt ->
  let empty_subst = [] in
  let rec unify_helper : (subst * bool) -> cbexp -> cbexp -> (subst * bool) = fun (subst, current_state) -> fun old -> fun tgt ->
    if(not current_state) then
      (empty_subst, false)
    else
      match (old, tgt) with
      | (CONST b1, CONST b2)  -> if(b1 = b2) then (subst, true) else (empty_subst, false)
      | (CONST _, _) -> (empty_subst, false)
      | (CVAR x1, CVAR x2) -> if(x1 = x2) then (subst, true) else (empty_subst, false)
      | (CVAR _, _) -> (empty_subst, false)
      | (VAR x1, CVAR x2) -> ((x1, CVAR x2)::subst, true)
      | (VAR x, _) -> (empty_subst, false)
      | (AND (b1, b2), AND(b3, b4))  
      | (OR (b1, b2), OR(b3, b4))  
      | (XOR (b1, b2), XOR(b3, b4)) -> 
        let (subst_with_b1, state_b1) = unify_helper (subst, current_state) b1 b3 in
        let (subst_with_b1_b2, state_b1_b2) = unify_helper (subst_with_b1, state_b1) (substitute_cbexp subst_with_b1 b2) b4 in
        (subst_with_b1_b2, state_b1_b2)
      | (NOT b1, NOT b2) -> 
        let (subst_with_b1, state_b1) = unify_helper (subst, current_state) b1 b2 in
        (subst_with_b1, state_b1)
      | _ ->
	      (empty_subst, false) 
	in
  let (result_subst, success) = unify_helper (empty_subst, true) old tgt in
  (List.rev result_subst, success)
 

(*
let unify : G.t -> bexp -> G.vertex -> subst = fun graph -> fun old -> fun tgt_node ->
  let empty_subst = [] in
  let rec unify_helper : (bexp * bexp) list -> bexp -> G.vertex -> (bexp * bexp) list = 
  fun subst -> fun old -> fun tgt_node ->
    match (old, (fst tgt_node)) with
    | (NULL, _) -> raise (Error "null is used")
    | (CONST b1, Node.CONST b2) -> subst (*if (b1 = b2) then subst else subst @ [((CONST b1), (CONST b2))] *)
    | (CONST b, Node.VAR x) ->  subst (* @ [(CONST b, VAR x)] *)
    | (CONST x, _) -> let tgt_bexp = get_bexp_of_node graph tgt_node in subst (*@ [(CONST x, tgt_bexp)]*)
    | (VAR x, Node.CONST b) -> subst (*@ [(VAR x, CONST b)] *)
    | (VAR x1, Node.VAR x2) -> if (x1 = x2) then subst else subst @ [(VAR x1, VAR x2)]
    | (VAR x, _) -> let tgt_bexp = get_bexp_of_node graph tgt_node in subst @ [(VAR x, tgt_bexp)]
    | (AND (b1, b2), Node.AND)  
    | (OR (b1, b2), Node.OR)  
    | (XOR (b1, b2), Node.XOR) -> 
      let pred = G.pred graph tgt_node in
      let tgt_b1 = List.nth pred 0 in
      let tgt_b2 = List.nth pred 1 in
      let subst_with_b1 = unify_helper subst b1 tgt_b1 in
      let subst_with_b1_b2 = unify_helper subst_with_b1 (substitute_cbexp subst_with_b1 b2) tgt_b2 in
      subst_with_b1_b2
    | (NOT b1, Node.NOT) -> 
      let pred = G.pred graph tgt_node in
      let tgt_b1 = List.nth pred 0 in
      let subst_with_b1 = unify_helper subst b1 tgt_b1 in
      subst_with_b1
    | (_, Node.VAR x) -> 
      if (snd tgt_node = 0) then 
        subst @ [(old, VAR x)] 
      else
        let pred = G.pred graph tgt_node in
        if(List.length pred = 1) then unify_helper subst old (List.nth pred 0) else raise (Error ("unify error : invalid var " ^ x))
    | (_, Node.CONST x) -> subst (*@ [(old, CONST x)]*)
    | _ -> let tgt_bexp = get_bexp_of_node graph tgt_node in subst @ [(old, tgt_bexp)]
  in
  unify_helper empty_subst old tgt_node

*)
(*
let rec unify : G.t -> bexp -> G.vertex -> subst = fun graph -> fun old -> fun tgt_node ->
  match (old, (fst tgt_node)) with
  | (NULL, _) -> raise (Error "null is used")
  | (CONST b1, Node.CONST b2) -> Whole(CONST b1, CONST b2)
  | (CONST b, Node.VAR x) ->  Whole (CONST b, VAR x)
  | (CONST b, _) -> let tgt_bexp = get_bexp_of_node graph tgt_node in Whole (CONST b, tgt_bexp)
  | (VAR x, Node.CONST b) -> Whole(VAR x, CONST b)
  | (VAR x1, Node.VAR x2) -> Whole(VAR x1, VAR x2)
  | (VAR x, _) -> let tgt_bexp = get_bexp_of_node graph tgt_node in Whole (VAR x, tgt_bexp)
  | (AND (b1, b2), Node.AND)  
  | (OR (b1, b2), Node.OR)  
  | (XOR (b1, b2), Node.XOR) -> 
    let pred = G.pred graph tgt_node in
    let tgt_b1 = List.nth pred 0 in
    let tgt_b2 = List.nth pred 1 in
    let subst_with_b1 = unify graph b1 tgt_b1 in
    let subst_with_b2 = unify graph b2 tgt_b2 in
    Bi(subst_with_b1, subst_with_b2)
  | (NOT b1, Node.NOT) -> 
    let pred = G.pred graph tgt_node in
    let tgt_b1 = List.nth pred 0 in
    let subst_with_b1 = unify graph b1 tgt_b1 in
    Bi(subst_with_b1, subst_with_b1)
  | (_, Node.VAR x) -> 
    if (snd tgt_node = 0) then 
      Whole (old, VAR x)
    else
      let pred = G.pred graph tgt_node in
      if(List.length pred = 1) then 
        unify graph old (List.nth pred 0) 
      else 
        raise (Error ("unify error : invalid var " ^ x))
  | (_, Node.CONST b) -> Whole(old, CONST b)
  | _ -> let tgt_bexp = get_bexp_of_node graph tgt_node in Whole(old, tgt_bexp)
*)




