open Circuit
open Graph_circuit


let rec alpha_subst : (var, var) BatMap.t -> bexp -> bexp = fun subst -> fun bexp ->
  match bexp with
  | NULL -> raise (Error "null is used")
  | CONST _ -> bexp
  | VAR x -> if (BatMap.mem x subst) then VAR (BatMap.find x subst) else bexp 
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


let rec alpha_conv : (bexp * bexp) -> (bexp * bexp) = fun (old_bexp, new_bexp) ->
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
  | (OR (a, b), OR(c, d)) -> ((is_equal_bexp a c) && (is_equal_bexp b d)) || ((is_equal_bexp a d) && (is_equal_bexp b c))
  | (NOT b1, NOT b2) -> is_equal_bexp b1 b2
  | _ -> false


let rec mem_subst : bexp -> (bexp * bexp) list -> bool = fun key -> fun subst ->
  match subst with
  | [] -> false 
  | (b1, b2)::tl -> if(is_equal_bexp b1 key) then true else mem_subst key tl

let rec find_subst : bexp -> (bexp * bexp) list -> bexp = fun key -> fun subst ->
  match subst with
  | [] -> raise (Error "Key Not Found")
  | (b1, b2)::tl -> if(is_equal_bexp b1 key) then b2 else find_subst key tl



(*
let rec substitute_bexp : subst -> bexp -> bexp = fun subst -> fun bexp ->
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
        let subst_b1 = substitute_bexp s1 b1 in
        let subst_b2 = substitute_bexp s2 b2 in
        let b1b2 = AND (subst_b1, subst_b2) in
        b1b2
    | OR (b1, b2) -> 
        let subst_b1 = substitute_bexp s1 b1 in
        let subst_b2 = substitute_bexp s2 b2 in
        let b1b2 = OR (subst_b1, subst_b2) in
        b1b2
     | XOR (b1, b2) -> 
        let subst_b1 = substitute_bexp s1 b1 in
        let subst_b2 = substitute_bexp s2 b2 in
        let b1b2 = XOR (subst_b1, subst_b2) in
        b1b2
     | NOT b1 -> 
        let subst_b1 = substitute_bexp s1 b1 in
        let nb1 = NOT subst_b1 in
        nb1
  *) 



let rec substitute_bexp : (var, bexp) BatMap.t -> bexp -> bexp = fun subst -> fun bexp ->
  match bexp with
  | NULL -> raise (Error "null is used")
  | CONST _ -> bexp
  | VAR x -> if (BatMap.mem x subst) then (BatMap.find x subst) else bexp 
  | AND (b1, b2) -> 
    let alpha_b1 = substitute_bexp subst b1 in
    let alpha_b2 = substitute_bexp subst b2 in
    AND (alpha_b1, alpha_b2)
  | OR (b1, b2) -> 
    let alpha_b1 = substitute_bexp subst b1 in
    let alpha_b2 = substitute_bexp subst b2 in
    OR (alpha_b1, alpha_b2)
  | XOR (b1, b2) -> 
    let alpha_b1 = substitute_bexp subst b1 in
    let alpha_b2 = substitute_bexp subst b2 in
    XOR (alpha_b1, alpha_b2)
  | NOT b1 -> 
    let alpha_b1 = substitute_bexp subst b1 in
    NOT alpha_b1



(*
let rec substitute_bexp : (bexp * bexp) list -> bexp -> bexp = fun subst -> fun bexp ->
  (*let select_index = let _ = Random.self_init() in Random.int 500000 in
  let _ = if(select_index < 2) then ((print_subst subst);(Pp.print_bexp bexp);print_newline()) else () in*)
  match bexp with
  | NULL -> raise (Error "null is used")
  | CONST _ | VAR _ -> if (mem_subst bexp subst) then find_subst bexp subst else bexp
  | AND (b1, b2) -> 
    let subst_b1 = substitute_bexp subst b1 in
    let subst_b2 = substitute_bexp subst b2 in
    let b1b2 = AND (subst_b1, subst_b2) in
    if(mem_subst b1b2 subst) then find_subst b1b2 subst else b1b2
  | OR (b1, b2) -> 
    let subst_b1 = substitute_bexp subst b1 in
    let subst_b2 = substitute_bexp subst b2 in
    let b1b2 = OR (subst_b1, subst_b2) in
    if(mem_subst b1b2 subst) then find_subst b1b2 subst else b1b2
  | XOR (b1, b2) -> 
    let subst_b1 = substitute_bexp subst b1 in
    let subst_b2 = substitute_bexp subst b2 in
    let b1b2 = XOR (subst_b1, subst_b2) in
    if(mem_subst b1b2 subst) then find_subst b1b2 subst else b1b2
  | NOT b1 -> 
    let subst_b1 = substitute_bexp subst b1 in
    let nb1 = NOT subst_b1 in
    if(mem_subst nb1 subst) then find_subst nb1 subst else nb1
*)
 
(*  let vlist = get_bexp_varlist bexp in
  List.fold_left (fun acc -> fun v -> subst_once acc) bexp vlist
*)



let unify : G.t -> bexp -> G.vertex -> ((var, bexp) BatMap.t * bool) = fun graph -> fun old -> fun tgt_node ->
  let empty_subst = BatMap.empty in
  let rec unify_helper : ((var, bexp) BatMap.t * bool) -> bexp -> G.vertex -> ((var, bexp) BatMap.t * bool) = fun (subst, current_state) -> fun old -> fun tgt_node ->
    if(not current_state) then
      (empty_subst, false)
    else
      match (old, (fst tgt_node)) with
      | (NULL, _) -> raise (Error "null is used")
      | (CONST _, Node.CONST _) 
      | (CONST _, Node.VAR _) 
      | (CONST _, _) -> (subst, false)
      | (VAR x, Node.CONST b) -> 
        if(BatMap.mem x subst) then
          if(BatMap.find x subst = CONST b) then (subst, true) else (empty_subst, false)
        else 
          (BatMap.add (x) (CONST b) subst, true)
      | (VAR x1, Node.VAR x2) -> 
        if(BatMap.mem x1 subst) then
          if(BatMap.find x1 subst = VAR x2) then (subst, true) else (empty_subst, false)
        else 
          (BatMap.add (x1) (VAR x2) subst, true) 
      | (VAR x, _) -> 
        let tgt_bexp = get_bexp_of_node graph tgt_node in 
        if(BatMap.mem x subst) then
          if(is_equal_bexp (BatMap.find x subst) tgt_bexp) then (subst, true) else (empty_subst, false)
        else 
          (BatMap.add (x) (tgt_bexp) subst, true) 
      | (AND (b1, b2), Node.AND)  
      | (OR (b1, b2), Node.OR)  
      | (XOR (b1, b2), Node.XOR) -> 
        let pred = G.pred graph tgt_node in
        let tgt_b1 = List.nth pred 0 in
        let tgt_b2 = List.nth pred 1 in
        let (subst_with_b1, state_b1) = unify_helper (subst, current_state) b1 tgt_b1 in
        let (subst_with_b1_b2, state_b1_b2) = unify_helper (subst_with_b1, state_b1) b2 tgt_b2 in
        (subst_with_b1_b2, state_b1_b2)
      | (NOT b1, Node.NOT) -> 
        let pred = G.pred graph tgt_node in
        let tgt_b1 = List.nth pred 0 in
        let (subst_with_b1, state_b1) = unify_helper (subst, current_state) b1 tgt_b1 in
        (subst_with_b1, state_b1)
      | (_, Node.VAR x) -> 
        if (snd tgt_node = 0) then 
          (empty_subst, false)
        else
          let pred = G.pred graph tgt_node in
          if(List.length pred = 1) then unify_helper (subst, current_state) old (List.nth pred 0) else raise (Error ("unify error : invalid var " ^ x))
      | (_, Node.CONST x) -> (empty_subst, false)
      | _ -> (empty_subst, false)
    in
    unify_helper (empty_subst, true) old tgt_node
  


(*
let unify : G.t -> bexp -> G.vertex -> (bexp * bexp) list = fun graph -> fun old -> fun tgt_node ->
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
      let subst_with_b1_b2 = unify_helper subst_with_b1 (substitute_bexp subst_with_b1 b2) tgt_b2 in
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





(* assume : tgt is var node *)
let graph_opt_by_case : var list -> var list -> G.t -> G.vertex -> (bexp * bexp) -> (G.t * bool) = 
fun tgt_vlist -> fun ilist ->  fun graph -> fun tgt_node -> fun (old_bexp, new_bexp) ->
  let (old_bexp, new_bexp) = alpha_conv (old_bexp, new_bexp) in
  let (subst, is_success) = unify graph old_bexp tgt_node in
  (*let _ = print_subst subst in*)
  if(not is_success) then
    (graph, false)
  else
    let sub_old = substitute_bexp subst old_bexp in
    let sub_old_vlist = get_bexp_varlist sub_old in
    let tgt_bexp = get_vlist_expanded_bexp_of_node graph tgt_node sub_old_vlist in
    if(not (is_equal_bexp sub_old tgt_bexp)) then
      (graph, false)
    else
      let sub_new = substitute_bexp subst new_bexp in
      let before_depth = get_mult_depth_of_node graph tgt_node in
      (* replace graph tgt ==> S new *)
      let pred = G.pred graph tgt_node in
      let lv = match tgt_node with | (Node.VAR x, _) -> x | _ -> raise (Error "trying to opt non-var node") in
      let _ = if(List.length pred = 1) then () else raise (Error "trying to opt non-var node") in
      let graph_without_edge = G.remove_edge graph (List.nth pred 0) tgt_node in
      (*let graph_without_edge = G.remove_edge graph_without_edge tgt_node (List.nth pred 0) in*)
      let graph_with_new_tgt = add_eqn ilist graph_without_edge (lv, sub_new) in
      (*let _ = print_graph graph_with_new_tgt tg*)
      let after_depth = get_mult_depth_of_node graph_with_new_tgt tgt_node in
      if (after_depth >= before_depth) then
        (graph, false)
      else 
        let _ = print_endline("pattern matched!") in
        let _ = print_subst subst in
        let _ = print_string("\nold bexp : "); Pp.print_bexp old_bexp in
        let _ = print_string("\nnew bexp : "); Pp.print_bexp new_bexp in
        let _ = print_string("\ntgt bexp     : "); Pp.print_bexp tgt_bexp in
        let _ = print_string("\nsub old bexp : "); Pp.print_bexp sub_old in
        let _ = print_string("\nsub new bexp : "); Pp.print_bexp sub_new in
        let _ = print_endline ("\nbefore depth : " ^ (string_of_int before_depth)) in
        let _ = print_endline ("after depth : " ^ (string_of_int after_depth)) in
        let _ = print_endline ("graph replaced\n\n") in
        (*let _ = print_graph graph_with_new_tgt (get_graph_varlist graph_with_new_tgt) in*)
        (graph_with_new_tgt, true)
    

(*
(* assume : tgt is var node *)
let graph_opt_by_case : var list -> var list -> G.t -> G.vertex -> (bexp * bexp) -> G.t = 
fun tgt_vlist -> fun ilist ->  fun graph -> fun tgt_node -> fun (old_bexp, new_bexp) ->
  let (old_bexp, new_bexpa) = alpha_conv (old_bexp, new_bexp) in
  let subst = unify graph old_bexp tgt_node in
  (*let _ = print_subst subst in*)
  let sub_old = substitute_bexp subst old_bexp in
  let sub_old_vlist = get_bexp_varlist sub_old in
  let sub_new = substitute_bexp subst new_bexp in
  let sub_new_vlist = get_bexp_varlist sub_new in
  let new_vlist = get_bexp_varlist new_bexp in
  let is_valid_replace = ((List.length (list_inter sub_old_vlist new_vlist)) = 0) && (list_subset sub_old_vlist sub_new_vlist) && (list_subset sub_new_vlist sub_old_vlist) in
  if(not is_valid_replace) then
    graph
  else
    let before_depth = get_mult_depth_of_node graph tgt_node in
    let pred = G.pred graph tgt_node in
    let lv = match tgt_node with | (Node.VAR x, _) -> x | _ -> raise (Error "trying to opt non-var node") in
    let _ = if(List.length pred = 1) then () else raise (Error "trying to opt non-var node") in
    let graph_without_edge = G.remove_edge graph (List.nth pred 0) tgt_node in
    (*let graph_without_edge = G.remove_edge graph_without_edge tgt_node (List.nth pred 0) in*)
    let graph_with_new_tgt = add_eqn ilist graph_without_edge (lv, sub_new) in
    (*let _ = print_graph graph_with_new_tgt tgt_vlist in*)
    let after_depth = get_mult_depth_of_node graph_with_new_tgt tgt_node in
    if (after_depth < before_depth) then 
      let tgt_bexp = get_vlist_expanded_bexp_of_node graph tgt_node sub_old_vlist in
      let _ = print_endline("pattern matched!") in
      let _ = print_subst subst in
      let _ = print_string("\nold bexp : "); Pp.print_bexp old_bexp in
      let _ = print_string("\nnew bexp : "); Pp.print_bexp new_bexp in
      let _ = print_string("\ntgt bexp     : "); Pp.print_bexp tgt_bexp in
      let _ = print_string("\nsub old bexp : "); Pp.print_bexp sub_old in
      let _ = print_string ("\nis_tgt_subold_same : " ^ (string_of_bool (is_equal_bexp sub_old tgt_bexp))) in
      let _ = print_string("\nsub new bexp : "); Pp.print_bexp sub_new in
      let _ = print_endline ("\nbefore depth : " ^ (string_of_int before_depth)) in
      let _ = print_endline ("after depth : " ^ (string_of_int after_depth)) in
      let _ = print_endline ("graph replaced\n\n") in
      (*let _ = print_graph graph_with_new_tgt (get_graph_varlist graph_with_new_tgt) in*)
      graph_with_new_tgt 
    else
      graph
*)  
  
let node_opt_by_case_list : var list -> var list -> G.t -> G.vertex -> (bexp * bexp) list -> (G.t * bool) = fun tgt_vlist -> fun ilist -> fun graph -> fun tgt_node -> fun case_list ->
  (*let _ = print_endline("node_opt is called " ^ (string_of_int (List.length case_list))) in*)
  List.fold_left 
  (fun (acc_graph, acc_success) -> fun case -> 
    let (new_graph, success) = graph_opt_by_case tgt_vlist ilist acc_graph tgt_node case in
    (new_graph, (success || acc_success))
  ) 
  (graph, false) 
  case_list
 
let commutative_bexp_list : bexp -> bexp list = fun bexp -> 
  let rec commutative_bexp_list : bexp -> int -> (bexp list * int) = fun bexp -> fun num ->
    if(num > 12) then 
      ([bexp], num) 
    else
      match bexp with
      | NULL -> raise (Error "null is used")
      | CONST _ | VAR _ -> ([bexp], num)
      | AND (b1, b2) -> 
        let (com_b1, num_b1) = commutative_bexp_list b1 (num+1) in
        let (com_b2, num_b2) = commutative_bexp_list b2 num_b1 in
        let combination = List.map (fun b1 -> (List.map (fun b2 -> ((b1, b2)::(b2, b1)::[])) com_b2)) com_b1 in
        let flatten : (bexp * bexp) list = List.concat (List.concat combination) in
        (List.map (fun (b1, b2) -> AND (b1, b2)) flatten, num_b2)
      | OR (b1, b2) -> 
        let (com_b1, num_b1) = commutative_bexp_list b1 (num+1) in
        let (com_b2, num_b2) = commutative_bexp_list b2 num_b1 in
        let combination = List.map (fun b1 -> (List.map (fun b2 -> ((b1, b2)::(b2, b1)::[])) com_b2)) com_b1 in
        let flatten : (bexp * bexp) list = List.concat (List.concat combination) in
        (List.map (fun (b1, b2) -> OR (b1, b2)) flatten, num_b2)
      | XOR (b1, b2) -> 
        let (com_b1, num_b1) = commutative_bexp_list b1 (num+1) in
        let (com_b2, num_b2) = commutative_bexp_list b2 num_b1 in
        let combination = List.map (fun b1 -> (List.map (fun b2 -> ((b1, b2)::(b2, b1)::[])) com_b2)) com_b1 in
        let flatten : (bexp * bexp) list = List.concat (List.concat combination) in
        (List.map (fun (b1, b2) -> XOR (b1, b2)) flatten, num_b2)
      | NOT b1 -> 
        let (com_b1, num_b1) = commutative_bexp_list b1 num in
        (List.map (fun b1 -> NOT b1) com_b1, num_b1)
  in
  (fst (commutative_bexp_list bexp 0))

let commutative_case_list : (bexp * bexp) -> (bexp * bexp) list = fun (old_b, new_b) ->
  let com_old = commutative_bexp_list old_b in
  List.map (fun x -> (x, new_b)) com_old

let graph_opt_by_case_file : var list -> var list -> G.t -> string -> G.t = fun tgt_var_list -> fun ilist -> fun graph -> fun case_filename ->
  let input_case = open_in (case_filename) in
  let lexbuf_case = Lexing.from_channel input_case in
  let case_list = CaseParser.main CaseLexer.token lexbuf_case in
  (* TODO : commutative function *)
  let com_case_list_list = List.map (fun x -> commutative_case_list x) case_list in
  let com_case_list = (List.concat com_case_list_list) in
  let graph_opt_once : G.t -> (G.t * bool) = fun graph ->
    let _ = print_endline("\n\ngraph_opt once called") in
    List.fold_left
    (fun (acc_graph, acc_success) -> fun tgt_var ->
      let tgt_node = (Node.VAR tgt_var, 1) in
      let (new_graph, success) = node_opt_by_case_list tgt_var_list ilist acc_graph tgt_node com_case_list in
      (new_graph, (success || acc_success))
    )
    (graph, false)
    tgt_var_list
  in
  let rec opt_iter : (G.t * bool) -> (G.t * bool) = fun (acc_graph, acc_success) ->
    if(acc_success) then opt_iter (graph_opt_once acc_graph) else (acc_graph, false)
  in 
  let _ = print_endline("\n\ngraph_opt called") in
  fst (opt_iter (graph, true))

let graph_depth_print : G.t -> var list -> unit = fun graph -> fun olist ->
  let _ = print_endline("---graph depth print---") in
  let rec helper : var list -> unit = fun olist ->
    match olist with
    | [] -> print_endline("---------------------")
    | hd::tl -> 
      let node = (Node.VAR hd, 1) in
      let depth = get_mult_depth_of_node graph node in
      let _ = print_endline("var : " ^ hd ^ " depth : " ^ (string_of_int depth)) in
      helper tl
  in
  helper olist

