open Circuit
open Graph_circuit
open Unify

type vdmap = (var, int) BatMap.t

let commu_num = 5
let max_fail_iter = 10
let tmpvar_index = ref 0
let optimal_graph = ref G.empty
let optimal_vdmap = ref BatMap.empty

let new_var : unit -> string = fun unit -> 
  let new_index = !tmpvar_index in 
  let _ = tmpvar_index := !tmpvar_index + 1 in
  "tmp" ^ (string_of_int new_index)



let get_max_depth : vdmap -> int = fun vdmap ->
  BatMap.fold (fun value -> fun acc -> if(acc < value) then value else acc) vdmap 0
  
let graph_performace : G.t -> vdmap -> int = fun graph -> fun vdmap ->
  let depth = get_max_depth vdmap in
  let mult_gate = mult_size_of_graph graph in
  let _ = print_endline("graph mult size : " ^ (string_of_int mult_gate)) in
  depth * depth * depth * mult_gate 

let rec is_terminal : bexp -> bool = fun bexp ->
  match bexp with
  | CONST _
  | VAR _ -> true
  | AND (b1, b2) 
  | XOR (b1, b2)
  | OR (b1, b2) -> false
  | NOT b1 -> false
  | NULL -> raise (Error "NULL var is used")

let rec eqn_to_single_gate : eqn -> eqn list = fun eqn ->
  let (lv, bexp) = eqn in
  match bexp with
  | CONST _
  | VAR _ -> [eqn]
  | AND (b1, b2) ->
    if(is_terminal b1 && is_terminal b2) then 
      [eqn] 
    else
      let b1_lv = new_var() in
      let b1_eqnlist = eqn_to_single_gate (b1_lv, b1) in
      let b2_lv = new_var() in
      let b2_eqnlist = eqn_to_single_gate (b2_lv, b2) in
      b1_eqnlist @ b2_eqnlist @ [(lv, (AND ((VAR b1_lv), (VAR b2_lv))))]
  | XOR (b1, b2) ->
    if(is_terminal b1 && is_terminal b2) then 
      [eqn] 
    else
      let b1_lv = new_var() in
      let b1_eqnlist = eqn_to_single_gate (b1_lv, b1) in
      let b2_lv = new_var() in
      let b2_eqnlist = eqn_to_single_gate (b2_lv, b2) in
      b1_eqnlist @ b2_eqnlist @ [(lv, (XOR ((VAR b1_lv), (VAR b2_lv))))]
  | OR (b1, b2) -> 
    if(is_terminal b1 && is_terminal b2) then 
      [eqn] 
    else
      let b1_lv = new_var() in
      let b1_eqnlist = eqn_to_single_gate (b1_lv, b1) in
      let b2_lv = new_var() in
      let b2_eqnlist = eqn_to_single_gate (b2_lv, b2) in
      b1_eqnlist @ b2_eqnlist @ [(lv, (OR ((VAR b1_lv), (VAR b2_lv))))]
  | NOT b1 -> 
    if(is_terminal b1) then 
      [eqn] 
    else
      let b1_lv = new_var() in
      let b1_eqnlist = eqn_to_single_gate (b1_lv, b1) in
      b1_eqnlist @ [(lv, (NOT (VAR b1_lv)))]
  | NULL -> raise (Error "NULL var is used")   




let rec update_vdmap_helper : G.t -> vdmap -> (G.vertex list * G.vertex BatSet.t) -> vdmap = fun graph -> fun acc -> fun (todo, delayed_set) ->
  let ilist = get_graph_ilist graph in
  if(todo = []) then 
    if(BatSet.is_empty delayed_set) then acc else update_vdmap_helper graph acc (BatSet.to_list delayed_set, BatSet.empty)
  else
    let (new_acc, new_delayed) =
      List.fold_left
        (fun (acc, acc_delayed) -> fun vertex -> 
           match fst vertex with 
           | Node.CONST b -> (acc, acc_delayed)
           | Node.AND | Node.OR | Node.XOR | Node.NOT -> (acc, acc_delayed)
           | Node.VAR x -> 
             if(List.mem x ilist) then
               (BatMap.add x 0 acc, acc_delayed)
             else
               let bexp = get_bexp_of_var graph x in 
               let bexp_varlist = get_bexp_varlist bexp in
               let delay_check = List.filter (fun var -> not(BatMap.mem var acc)) bexp_varlist in
               if(delay_check = []) then
                 let depth = get_mult_depth_bexp_with_vdmap acc bexp in
                 (BatMap.add x depth acc, acc_delayed)
               else
                 (acc, BatSet.add vertex acc_delayed)
        )  
        (acc, BatSet.empty)
        todo
    in
    let todo_set = BatSet.of_list todo in
    let new_todo_root = BatSet.diff todo_set new_delayed in
    let new_todo_set = 
      List.fold_left
        (fun acc -> fun vertex ->
           let succ = BatSet.of_list (G.succ graph vertex) in
           BatSet.union succ acc
        )
        BatSet.empty
        (BatSet.to_list new_todo_root)
    in
    let new_todo = BatSet.to_list (new_todo_set) in
    let new_delayed = BatSet.union new_delayed delayed_set in
    update_vdmap_helper graph new_acc (new_todo, new_delayed)


let update_vdmap : G.t -> vdmap -> G.vertex list -> vdmap = fun graph -> fun acc -> fun todo ->
  update_vdmap_helper graph acc (todo, BatSet.empty)


let get_var_depth_map : G.t -> vdmap = fun graph ->
  let ilist = get_graph_ilist graph in
  let ilist_vertex = List.map (fun var -> (Node.VAR var, 0)) ilist in
  update_vdmap graph BatMap.empty ((Node.CONST true, 0)::(Node.CONST false, 0)::ilist_vertex)




let commutative_cbexp_list : cbexp -> cbexp list = fun cbexp ->
  let rec commutative_cbexp_list : cbexp -> int -> (cbexp list * int) = fun cbexp -> fun num ->
    if(num > commu_num) then
      ([cbexp], num)
    else
      match cbexp with
      | CONST _ | VAR _ | CVAR _ -> ([cbexp], num)
      | AND (b1, b2) ->
        let (com_b1, num_b1) = commutative_cbexp_list b1 (num+1) in
        let (com_b2, num_b2) = commutative_cbexp_list b2 num_b1 in
        let combination = List.map (fun b1 -> (List.map (fun b2 -> ((b1, b2)::(b2, b1)::[])) com_b2)) com_b1 in
        let flatten : (cbexp * cbexp) list = List.concat (List.concat combination) in
        (List.map (fun (b1, b2) -> Unify.AND (b1, b2)) flatten, num_b2)
      | OR (b1, b2) ->
        let (com_b1, num_b1) = commutative_cbexp_list b1 (num+1) in
        let (com_b2, num_b2) = commutative_cbexp_list b2 num_b1 in
        let combination = List.map (fun b1 -> (List.map (fun b2 -> ((b1, b2)::(b2, b1)::[])) com_b2)) com_b1 in
        let flatten : (cbexp * cbexp) list = List.concat (List.concat combination) in
        (List.map (fun (b1, b2) -> Unify.OR (b1, b2)) flatten, num_b2)
      | XOR (b1, b2) ->
        let (com_b1, num_b1) = commutative_cbexp_list b1 (num+1) in
        let (com_b2, num_b2) = commutative_cbexp_list b2 num_b1 in
        let combination = List.map (fun b1 -> (List.map (fun b2 -> ((b1, b2)::(b2, b1)::[])) com_b2)) com_b1 in
        let flatten : (cbexp * cbexp) list = List.concat (List.concat combination) in
        (List.map (fun (b1, b2) -> Unify.XOR (b1, b2)) flatten, num_b2)
      | NOT b1 ->
        let (com_b1, num_b1) = commutative_cbexp_list b1 num in
        (List.map (fun b1 -> Unify.NOT b1) com_b1, num_b1)
  in
  (fst (commutative_cbexp_list cbexp 0))


let commutative_bexp_list : bexp -> bexp list = fun bexp ->
  let rec commutative_bexp_list : bexp -> int -> (bexp list * int) = fun bexp -> fun num ->
    if(num > commu_num) then
      ([bexp], num)
    else
      match bexp with
      | NULL -> raise (Error "null is used")
      | CONST _ | VAR _ -> ([bexp], num)
      | AND (b1, b2) ->
        let (com_b1, num_b1) = commutative_bexp_list b1 (num+1) in
        let (com_b2, num_b2) = commutative_bexp_list b2 num_b1 in
        let combination = List.map (fun b1 -> (List.map (fun b2 -> ((b1, b2)::(b2, b1)::[])) com_b2)) com_b1 in
        let flatten : (bexp * bexp) list = List.flatten (List.flatten combination) in
        (List.map (fun (b1, b2) -> Circuit.AND (b1, b2)) flatten, num_b2)
      | OR (b1, b2) ->
        let (com_b1, num_b1) = commutative_bexp_list b1 (num+1) in
        let (com_b2, num_b2) = commutative_bexp_list b2 num_b1 in
        let combination = List.map (fun b1 -> (List.map (fun b2 -> ((b1, b2)::(b2, b1)::[])) com_b2)) com_b1 in
        let flatten : (bexp * bexp) list = List.flatten (List.flatten combination) in
        (List.map (fun (b1, b2) -> Circuit.OR (b1, b2)) flatten, num_b2)
      | XOR (b1, b2) ->
        let (com_b1, num_b1) = commutative_bexp_list b1 (num+1) in
        let (com_b2, num_b2) = commutative_bexp_list b2 num_b1 in
        let combination = List.map (fun b1 -> (List.map (fun b2 -> ((b1, b2)::(b2, b1)::[])) com_b2)) com_b1 in
        let flatten : (bexp * bexp) list = List.flatten (List.flatten combination) in
        (List.map (fun (b1, b2) -> Circuit.XOR (b1, b2)) flatten, num_b2)
      | NOT b1 ->
        let (com_b1, num_b1) = commutative_bexp_list b1 num in
        (List.map (fun b1 -> Circuit.NOT b1) com_b1, num_b1)
  in
  (fst (commutative_bexp_list bexp 0))



(* assume : tgt is var node *)
let graph_opt_by_case : string -> G.t -> vdmap -> G.vertex -> (bexp * bexp) -> (G.t * vdmap * bool) = fun input_filename -> fun graph -> fun vdmap -> fun tgt_node -> fun (old_bexp, new_bexp) ->
  let ilist = get_graph_ilist graph in
  let (old_cbexp, new_cbexp) = alpha_conv (old_bexp, new_bexp) in
  let (subst, is_success, tgt_bexp) = unify graph old_cbexp tgt_node in
  (*let _ = print_subst subst in
  let _ = Pp.print_bexp tgt_bexp in*)
  if(not is_success) then
    (graph, vdmap, false)
  else
    let sub_old_cbexp = substitute_cbexp subst old_cbexp in
    let sub_old = cbexp2bexp sub_old_cbexp in
    let sub_old_vlist = get_bexp_varlist sub_old in
    (*    let _ =
          let old_vlist = get_bexp_varlist old_bexp in
          let is_n200_tgt = match tgt_node with (Node.VAR "n200", 1) -> true | _ -> false in
          let tgt_pred = G.pred graph tgt_node in
          if (is_n200_tgt && List.mem "n192" old_vlist && List.mem "n199" sub_old_vlist) then
          (*List.mem "n199" sub_old_vlist && List.mem ("n189") sub_old_vlist && List.mem "n192" sub_old_vlist && List.mem "n195" sub_old_vlist) then *)
            let commu_old = commutative_bexp_list old_bexp in
            let _ = print_endline("commu length : " ^ string_of_int(List.length commu_old)) in
            let _ = print_subst subst in
            let _ = print_string ("old : "); Pp.print_bexp old_bexp; print_newline()  in
            let _ = print_string ("new : "); Pp.print_bexp new_bexp; print_newline()  in
            let _ = print_string ("tgt : "); Pp.print_bexp tgt_bexp; print_newline() in
            let _ = print_string ("sub old : "); Pp.print_bexp sub_old; print_newline() in
            let _ = print_string ("sub new : "); Pp.print_bexp sub_new; print_newline() in
            () 
          else 
          () 
          in*)
    (*let tgt_bexp = get_vlist_expanded_bexp_of_node graph tgt_node sub_old_vlist in*)
    if(not (is_equal_bexp sub_old tgt_bexp)) then
      let _ = print_endline("old tgt not same error report") in
      let _ = print_string ("old : "); Pp.print_bexp old_bexp; print_newline()  in
      let _ = print_string ("tgt : "); Pp.print_bexp tgt_bexp; print_newline() in
      let _ = print_string ("sub old : "); Pp.print_bexp sub_old; print_newline() in
      let _ = print_subst subst in
      raise (Error  "old tgt not same") (*(graph, false)*)
    else
      let commu_new = commutative_cbexp_list new_cbexp in
      let sub_new_list = List.map (fun newb -> cbexp2bexp (substitute_cbexp subst newb)) commu_new in
      let sub_new_valid_check sub_new = 
        let sub_new_vlist = get_bexp_varlist sub_new in
        (list_subset sub_old_vlist sub_new_vlist) && (list_subset sub_new_vlist sub_old_vlist)
      in
      (*let sub_new_cbexp = substitute_cbexp subst new_cbexp in
        let sub_new = cbexp2bexp sub_new_cbexp in
        let sub_new_vlist = get_bexp_varlist sub_new in
        let valid_replace = (list_subset sub_old_vlist sub_new_vlist) && (list_subset sub_new_vlist sub_old_vlist) in*)
      let valid_sub_new_list = List.filter sub_new_valid_check sub_new_list in
      if(valid_sub_new_list = []) then
        (graph, vdmap, false)
      else
        let sub_new = List.hd valid_sub_new_list in
        (* replace graph tgt ==> S new *)
        let pred = G.pred graph tgt_node in
        let lv = match tgt_node with | (Node.VAR x, _) -> x | _ -> raise (Error "trying to opt non-var node") in
        let _ = if(List.length pred = 1) then () else raise (Error "trying to opt non-var node") in
        let before_depth = BatMap.find lv vdmap in
        let graph_without_edge = G.remove_edge graph (List.nth pred 0) tgt_node in
        (*let graph_without_edge = G.remove_edge graph_without_edge tgt_node (List.nth pred 0) in*)
        let single_eqn = eqn_to_single_gate (lv, sub_new) in
        let graph_with_new_tgt = List.fold_left (fun acc -> fun eqn -> add_eqn ilist acc eqn) graph_without_edge single_eqn in
        (*let graph_with_new_tgt = add_eqn ilist graph_without_edge (lv, sub_new) in*)
        (*let _ = print_graph graph_with_new_tgt tg*)
        let new_vdmap = update_vdmap graph_with_new_tgt vdmap (List.map (fun x -> (Node.VAR x, 1)) (List.map fst single_eqn)) in
        let after_depth = get_mult_depth_bexp_with_vdmap vdmap sub_new in
        if (after_depth >= before_depth) then
          (graph, vdmap, false)
        else 
          (*let new_vdmap = update_vdmap graph_with_new_tgt vdmap [tgt_node] in*)
          let _ = Z3.z3_print input_filename sub_old sub_new in
          let is_same = Z3.execute_z3 input_filename in
          let _ = print_endline("pattern matched!") in
          let _ = print_endline("tgt, new_tgt same? : " ^ (string_of_bool is_same)) in 
          let _ = print_subst subst in
          let _ = print_string("\nold bexp : "); Pp.print_bexp old_bexp in
          let _ = print_string("\nnew bexp : "); Pp.print_bexp new_bexp in
          let _ = print_string("\ntgt bexp     : "); Pp.print_bexp tgt_bexp in
          let _ = print_string("\nsub old bexp : "); Pp.print_bexp sub_old in
          let _ = print_string("\nsub new bexp : "); Pp.print_bexp sub_new in
          let _ = print_endline ("\nbefore depth : " ^ (string_of_int before_depth)) in
          let _ = print_endline ("after depth : " ^ (string_of_int after_depth)) in
          (*let _ = print_graph graph_with_new_tgt (get_graph_varlist graph_with_new_tgt) in*)
          if(not is_same) then
            (graph, vdmap, false)
          else 
            let _ = print_endline ("graph replaced\n\n") in 
            (graph_with_new_tgt, new_vdmap, true)


(*
(* assume : tgt is var node *)
let graph_opt_by_case : var list -> var list -> G.t -> G.vertex -> (bexp * bexp) -> G.t = 
fun tgt_vlist -> fun ilist ->  fun graph -> fun tgt_node -> fun (old_bexp, new_bexp) ->
  let (old_bexp, new_bexpa) = alpha_conv (old_bexp, new_bexp) in
  let subst = unify graph old_bexp tgt_node in
  (*let _ = print_subst subst in*)
  let sub_old = substitute_cbexp subst old_bexp in
  let sub_old_vlist = get_bexp_varlist sub_old in
  let sub_new = substitute_cbexp subst new_bexp in
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

let node_opt_by_case_list : string -> G.t -> vdmap -> G.vertex -> (bexp * bexp) list -> (G.t * vdmap * bool) = fun input_filename -> fun graph -> fun vdmap -> fun tgt_node -> fun case_list ->
  (*let _ = print_endline("node_opt is called " ^ (string_of_int (List.length case_list))) in*)
  List.fold_left 
    (fun (acc_graph, acc_vdmap, acc_success) -> fun case -> 
       let (new_graph, new_vdmap, success) = graph_opt_by_case input_filename acc_graph acc_vdmap tgt_node case in
       (new_graph, new_vdmap, (success || acc_success))
    ) 
    (graph, vdmap, false) 
    case_list



let commutative_case_list : (bexp * bexp) -> (bexp * bexp) list = fun (old_b, new_b) ->
  let com_old = commutative_bexp_list old_b in
  List.map (fun x -> (x, new_b)) com_old



let rec get_max_depth_vlist : var list -> vdmap -> var list = fun vlist -> fun vdmap ->
  let rec helper vlist vdmap acc max = 
    match vlist with
    | [] -> acc
    | hd::tl -> 
      let hd_depth = BatMap.find hd vdmap in  
      if(hd_depth < max) then 
        helper tl vdmap acc max        
      else if (hd_depth = max) then 
        helper tl vdmap (hd::acc) max
      else 
        helper tl vdmap [hd] hd_depth 
  in
  helper vlist vdmap [] 0


let find_critical_parents : G.t -> vdmap -> var -> var list = fun graph -> fun vdmap -> fun v ->
  let ilist = get_graph_ilist graph in
  if(List.mem v ilist) then
    []
  else
    let v_bexp = get_bexp_of_var graph v in
    let vlist = get_bexp_varlist v_bexp in
    get_max_depth_vlist vlist vdmap 


let find_oneof_critical_path : G.t -> vdmap -> var list = fun graph -> fun vdmap ->
  let olist = get_graph_olist graph in
  let critical_outputs = get_max_depth_vlist olist vdmap in
  let select_index = let _ = Random.self_init() in Random.int (List.length critical_outputs) in
  let root = List.nth critical_outputs select_index in
  let rec critical_helper var acc =
    let parents = find_critical_parents graph vdmap var in
    match parents with
    | [] -> acc
    | _ ->
      let select_index = let _ = Random.self_init() in Random.int (List.length parents) in
      let selected_parent = List.nth parents select_index in
      critical_helper selected_parent (var::acc)
  in
  (critical_helper root [])


let case_reducable : (bexp * bexp) -> (bexp * bexp) -> bool = fun (old1, new1) -> fun (old2, new2) ->
  let (oldc1, newc1) = (bexp2cbexp_onlytype old1, bexp2cbexp_onlytype new1) in
  let (oldc2, newc2) = (bexp2cbexp old2, bexp2cbexp new2) in
  let (subst, success) = unify_cbexp oldc1 oldc2 in
  if(not success) then
    false
  else
    let commu_new = commutative_cbexp_list newc1 in
    let sub_new_list = List.map (fun newc -> (substitute_cbexp subst newc)) commu_new in
    let valid_reduce_check_list = List.filter (fun newc -> is_equal_cbexp newc newc2) sub_new_list in
    if(valid_reduce_check_list = []) then
      false
    else
      true


let case_reducable_commutative : (bexp * bexp) -> (bexp * bexp) -> bool = fun case1 -> fun case2 ->
  (*let old1_size = get_bexp_size old1 in
    let old2_size = get_bexp_size old2 in
    let case1 = if (old1_size < old2_size) then (old2, new2) else (old1, new1) in
    let case2 = if (old1_size < old2_size) then (old1, new1) else (old2, new2) in*)
  let commu_case1 = commutative_case_list case1 in
  let reducable_list = List.filter (fun case1 -> case_reducable case1 case2) commu_case1 in
  if(reducable_list = []) then false else true


(* input case list : size descending order *)
let case_reduce : (bexp * bexp) list -> (bexp * bexp) list = fun case_list ->
  let rec tail_helper : (bexp * bexp) list -> (bexp * bexp) list -> (bexp * bexp) list = fun case_list -> fun acc -> 
    match case_list with
    | [] -> acc
    | hd::tl -> 
      let (old1, new1) = hd in
      let hd_required = 
        List.fold_left
          (fun acc_required -> fun (old2, new2) ->
             let small_case = (old2, new2) in
             let big_case = (old1, new1) in
             let reducable = case_reducable_commutative small_case big_case in
             let new_required = 
               if(not acc_required) then 
                 false
               else if (not reducable) then 
                 true 
               else 
                 false
             in
             new_required
          )
          true
          tl
      in
      if(hd_required) then tail_helper tl (hd::acc) else tail_helper tl acc
  in
  tail_helper case_list []


let graph_opt_by_case_file : G.t -> string -> string -> G.t = fun graph -> fun graph_filename -> fun case_filename ->
  let start_time = Unix.gettimeofday() in
  let _ = print_endline("============================================= graph opt start ===================================================") in
  let _ = print_endline ("initial graph size : " ^ string_of_int (size_of_graph graph)) in
  let _ = optimal_graph := graph in
  let input_case = open_in (case_filename) in
  let lexbuf_case = Lexing.from_channel input_case in
  let case_list = CaseParser.main CaseLexer.token lexbuf_case in
  (*let case_list = List.map (Normalize.normalize_case) case_list in*)
  let case_compare (old1, new1) (old2, new2) = 
    let size1 = get_bexp_size old1 in 
    let size2 = get_bexp_size old2 in
    if(size1 < size2) then 1 else if (size1 = size2) then 0 else -1
  in
  let case_list = List.sort case_compare case_list in
  let _ = print_endline ("whole case num : " ^ string_of_int (List.length case_list)) in
  let case_list = case_reduce case_list in
  let _ = print_endline ("reduced case num : " ^ string_of_int (List.length case_list)) in
  (*let unbal_case = List.filter (fun (oldb, newb) -> List.length(get_bexp_varlist oldb) != List.length (get_bexp_varlist newb)) case_list in
    let _ = 
    let _ = List.map (fun (oldb, newb) -> Pp.print_bexp oldb; print_string (" --> " ); Pp.print_bexp newb; print_newline()) unbal_case in  
    print_endline ("unbalanced case num : " ^ string_of_int (List.length unbal_case)) 
    in
  *)
  let com_case_list_list = List.map (fun x -> commutative_case_list x) case_list in
  (*let com_case_list = (List.flatten com_case_list_list) in*)
  let com_case_list = case_list in
  let initial_vdmap = get_var_depth_map graph in
  let _ = optimal_graph := graph; optimal_vdmap := initial_vdmap in
  let graph_opt_once : G.t -> vdmap -> (var list) -> (G.t * vdmap * bool) = fun graph -> fun vdmap -> fun todo ->
    List.fold_left
      (fun (acc_graph, acc_vdmap, acc_success) -> fun tgt_var ->
        let tgt_node = (Node.VAR tgt_var, 1) in
        let (new_graph, new_vdmap, success) = node_opt_by_case_list graph_filename acc_graph acc_vdmap tgt_node com_case_list in
        (new_graph, new_vdmap, (success || acc_success))
      )
      (graph, vdmap, false)
      todo
  in
  (*let ilist = get_graph_ilist graph in
    let olist = get_graph_olist graph in
    let output_num = List.length olist in
    let pred_varlist graph v = get_bexp_varlist (get_bexp_of_var graph v) in
    let rec opt_iter : (G.t * vdmap * bool) -> var list -> (G.t * vdmap * bool) = fun (acc_graph, acc_vdmap, acc_success) -> fun todo ->
    if (todo = []) then
      let _ = print_endline("graph opt iter finished") in
      let size = size_of_graph acc_graph in
      let _ = print_endline("============================================= graph opt iter once ===================================================") in 
      let _ = print_endline ("now graph size : " ^ string_of_int size) in
      if(acc_success) then opt_iter (acc_graph, acc_vdmap, false) olist else (acc_graph, acc_vdmap, false)
    else
      let pred_todo = List.fold_left (fun acc -> fun x -> list_merge acc (pred_varlist graph x)) [] todo in
      let new_todo = List.filter (fun x -> not (List.mem x ilist)) pred_todo in
      let (new_graph, new_vdmap, is_success) = graph_opt_once acc_graph acc_vdmap todo in
      opt_iter (new_graph, new_vdmap, (acc_success || is_success)) new_todo   
    in
  *)
  let rec opt_iter : (G.t * vdmap * bool) -> int -> var list -> (G.t * vdmap * bool) = fun (acc_graph, acc_vdmap, acc_success) -> fun fail_num -> fun todo ->
    let old_depth = get_max_depth acc_vdmap in
    if (todo = []) then
      let _ = print_endline("graph opt iter finished") in
      let size = size_of_graph acc_graph in
      let _ = print_endline("============================================= graph opt iter once ===================================================") in 
      let _ = print_endline ("now graph size : " ^ string_of_int size) in
      let new_cripath = (find_oneof_critical_path acc_graph acc_vdmap) in
      let random_index = let _ = Random.self_init() in Random.int 2 in
      let new_cripath = if(random_index mod 2 = 1) then new_cripath else List.rev new_cripath in
      let _ = print_endline("selected cripath : "); Pp.print_varlist new_cripath; print_newline() in
      let now_time = Unix.gettimeofday () in
      if(now_time -. start_time > 100000.) then
        (acc_graph, acc_vdmap, false)
      else if(acc_success) then 
        opt_iter (acc_graph, acc_vdmap, false) 0 new_cripath 
      else if(max_fail_iter < fail_num) then 
        (acc_graph, acc_vdmap, false)
      else
        opt_iter (acc_graph, acc_vdmap, false) (fail_num + 1) new_cripath 
    else
      let (new_graph, new_vdmap, is_success) = graph_opt_once acc_graph acc_vdmap todo in
      let new_depth = get_max_depth new_vdmap in
      let _ = 
        if(new_depth < old_depth) then 
          let cost = (graph_performace new_graph new_vdmap) in
          let _ = print_endline ("optimal graph replaced, cost : " ^ string_of_int(cost)) in
          (optimal_graph := new_graph; optimal_vdmap := new_vdmap)
        else () 
      in
      opt_iter (new_graph, new_vdmap, (acc_success || is_success)) fail_num [] 
  in
  let _ = print_endline("\n\ngraph_opt called") in
  (*let (opted_graph, _, _) = (opt_iter (graph, initial_vdmap, false) olist) in*)
  let (opted_graph, _, _) = (opt_iter (graph, initial_vdmap, true) 0 []) in
  let opted_graph = !optimal_graph in
  let opted_result_filename = graph_filename ^ "_opted_result" in
  let _ = Eqn_printer.printf_graph opted_result_filename opted_graph in
  opted_graph

let graph_depth_print : G.t -> var list -> unit = fun graph -> fun olist ->
  let _ = print_endline("---graph depth print---") in
  let vdmap = get_var_depth_map graph in
  let rec helper : var list -> unit = fun olist ->
    match olist with
    | [] -> print_endline("---------------------")
    | hd::tl -> 
      let depth = BatMap.find hd vdmap in
      let _ = print_endline("var : " ^ hd ^ " depth : " ^ (string_of_int depth)) in
      helper tl
  in
  helper olist

let graph_max_depth : G.t -> var list -> int = fun graph -> fun olist ->
  let vdmap = get_var_depth_map graph in
  let rec helper : var list -> int -> int = fun olist -> fun acc ->
    match olist with
    | [] -> acc
    | hd::tl ->
      let depth = BatMap.find hd vdmap in
      if(depth < acc) then helper tl acc else helper tl depth
  in
  helper olist 0
