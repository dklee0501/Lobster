open Circuit

module Node = struct
   type nodeVal = AND | OR | NOT | XOR | VAR of string | CONST of bool
   type label = int (* gate index(only to construct) *)
   type t = nodeVal * label
   let compare = Pervasives.compare
   let hash = Hashtbl.hash
   let equal = (=)
   (*let to_string node = 
     match node with
     | (AND, i) -> "(AND, " ^ string_of_int i ^ ")"*)
end

(* a functional/persistent graph *)
module G = Graph.Persistent.Digraph.ConcreteBidirectional(Node)

type vdmap = (var, int) BatMap.t

(*
let var_depth_map = ref BatMap.empty

let init_var_depth_map : G.t -> var list -> unit = fun graph -> fun vlist ->
  let current_map = !var_depth_map in
  match vlist with
  | hd::tl -> ()
  | [] -> ()
*)

let print_node : G.vertex -> unit = fun node ->
  let _ = print_string("nodeVal : ") in
  let _ =
    match fst node with
    | Node.AND -> print_string("AND")
    | Node.OR -> print_string("OR")
    | Node.XOR -> print_string("XOR")
    | Node.NOT -> print_string("NOT")
    | Node.VAR x -> print_string x
    | Node.CONST b -> print_string (string_of_bool b)
  in
  let _ = print_string (" index : " ^ string_of_int (snd node)) in
  ()
(*
let print_subst : (bexp * bexp) list -> unit = fun subst ->
  let _ = print_endline("----print_subst start----") in
  let _ = List.map (fun (old_bexp, new_bexp) -> Pp.print_bexp old_bexp; print_string("  -->  "); Pp.print_bexp new_bexp; print_newline()) subst in
  print_endline("---------------------")
*)

let print_subst : (var, bexp) BatMap.t -> unit = fun subst ->
  let _ = print_endline("----print_subst start----") in
  let _ = BatMap.iter (fun var -> fun new_bexp -> print_string("var " ^ var ^ "  -->  "); Pp.print_bexp new_bexp; print_newline()) subst in
  print_endline("---------------------")


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

let rec remove_redundant_bexp : bexp -> bexp = fun bexp ->
  match bexp with
  | NULL -> raise (Error "null used")
  | CONST b -> bexp
  | VAR x -> bexp
  | XOR (b1, b2) -> 
    let new_b1 = remove_redundant_bexp b1 in
    let new_b2 = remove_redundant_bexp b2 in
    if(is_equal_bexp new_b1 new_b2) then (CONST false) else XOR(new_b1, new_b2)
  | AND (b1, b2) -> 
    let new_b1 = remove_redundant_bexp b1 in
    let new_b2 = remove_redundant_bexp b2 in
    if(is_equal_bexp new_b1 new_b2) then (new_b1) else AND(new_b1, new_b2)
  | OR (b1, b2) -> 
    let new_b1 = remove_redundant_bexp b1 in
    let new_b2 = remove_redundant_bexp b2 in
    if(is_equal_bexp new_b1 new_b2) then (new_b1) else OR(new_b1, new_b2)
  | NOT (b1) -> 
    let new_b1 = remove_redundant_bexp b1 in
    NOT(new_b1)


let is_op_node : G.vertex -> bool = fun v ->
  match fst v with
  | Node.AND | Node.OR | Node.NOT | Node.XOR -> true
  | _ -> false

let is_mult_node : G.vertex -> bool = fun v ->
  match fst v with
  | Node.AND | Node.OR -> true
  | _ -> false

let is_var_node : G.vertex -> bool = fun v ->
  match fst v with
  | Node.VAR x -> true
  | _ -> false

let get_var_of_node : G.vertex -> var = fun v ->
  match fst v with
  | Node.VAR x -> x
  | _ -> raise (Error "getting var of non-var node")


let add_eqn : var list -> G.t -> eqn -> G.t = fun ilist -> fun graph -> fun eqn ->
  let (lv, bexp) = eqn in
  let new_bexp = remove_redundant_bexp bexp in
  let root_node = (Node.VAR lv, 1) in
  let graph_with_root = G.add_vertex graph root_node in
  let rec helper : G.t -> G.vertex -> bexp -> G.t = fun graph -> fun parent -> fun bexp ->
    match bexp with
    | NULL -> raise (Error "NULL var is used")
    | CONST b -> 
      let new_node = (Node.CONST b, 0) in
      let graph_with_node = G.add_vertex graph new_node in
      let graph_with_edge = G.add_edge graph_with_node new_node parent in
      graph_with_edge
    | VAR x -> 
      let var_index = if (List.mem x ilist) then 0 else 1 in
      let new_node = (Node.VAR x, var_index) in
      let graph_with_node = G.add_vertex graph new_node in
      let graph_with_edge = G.add_edge graph_with_node new_node parent in
      graph_with_edge
    | AND (b1, b2) ->
      let new_node = (Node.AND, G.nb_vertex graph) in
      let graph_with_node = G.add_vertex graph new_node in
      let graph_with_edge = G.add_edge graph_with_node new_node parent in
      let graph_with_b1 = helper graph_with_edge new_node b1 in
      let graph_with_b1_b2 = helper graph_with_b1 new_node b2 in
      graph_with_b1_b2
    | OR (b1, b2) ->
      let new_node = (Node.OR, G.nb_vertex graph) in
      let graph_with_node = G.add_vertex graph new_node in
      let graph_with_edge = G.add_edge graph_with_node new_node parent in
      let graph_with_b1 = helper graph_with_edge new_node b1 in
      let graph_with_b1_b2 = helper graph_with_b1 new_node b2 in
      graph_with_b1_b2
    | XOR (b1, b2) ->
      let new_node = (Node.XOR, G.nb_vertex graph) in
      let graph_with_node = G.add_vertex graph new_node in
      let graph_with_edge = G.add_edge graph_with_node new_node parent in
      let graph_with_b1 = helper graph_with_edge new_node b1 in
      let graph_with_b1_b2 = helper graph_with_b1 new_node b2 in
      graph_with_b1_b2
    | NOT b1 ->
      let new_node = (Node.NOT, G.nb_vertex graph) in
      let graph_with_node = G.add_vertex graph new_node in
      let graph_with_edge = G.add_edge graph_with_node new_node parent in
      let graph_with_b1 = helper graph_with_edge new_node b1 in
      graph_with_b1
  in
  helper graph_with_root root_node new_bexp

let cir2graph : circuit -> G.t = fun cir ->
  let (ilist, olist, elist) = cir in
  let empty_graph = G.empty in
  let graph_with_const = G.add_vertex (G.add_vertex empty_graph (Node.CONST true, 0)) (Node.CONST false, 0) in
  let graph_with_eqns = 
    List.fold_left
    (fun acc -> fun eqn -> 
      add_eqn ilist acc eqn) 
    graph_with_const
    elist 
  in
  graph_with_eqns

let is_valid_node : G.t -> G.vertex -> bool = fun graph -> fun node ->
  let pred_list = G.pred graph node in
  let pred_num = List.length pred_list in
  match node with
  | (Node.AND, _) | (Node.OR, _) | (Node.XOR, _) -> pred_num = 2
  | (Node.NOT, _) | (Node.VAR _, 1) -> pred_num = 1
  | (Node.VAR _, 0) -> (pred_num = 0)
  | (Node.CONST _, _) -> pred_num = 0
  | _ -> false


let get_graph_varlist : G.t -> var list = fun graph ->
  G.fold_vertex (fun v -> fun acc -> 
    match v with 
    | (Node.VAR x, 1) -> x::acc
    | _ -> acc) graph []

let get_graph_ilist : G.t -> var list = fun graph ->
  G.fold_vertex (fun v -> fun acc -> 
    match v with 
    | (Node.VAR x, 0) -> x::acc
    | _ -> acc) graph []


let get_graph_olist : G.t -> var list = fun graph ->
  G.fold_vertex (fun v -> fun acc -> 
    match v with 
    | (Node.VAR x, 1) -> if(x.[0] = 'm' || x.[0] = 'o') then x::acc else acc
    | _ -> acc)
  graph []

let get_graph_ovlist : G.t -> G.vertex list = fun graph ->
  G.fold_vertex (fun v -> fun acc -> 
    match v with 
    | (Node.VAR x, 1) -> if(x.[0] = 'm' || x.[0] = 'o') then v::acc else acc
    | _ -> acc)
  graph []


let size_of_graph : G.t -> int = fun graph ->
  let olist_set = BatSet.of_list (get_graph_ovlist graph) in
  let rec size_helper : G.vertex BatSet.t -> G.vertex BatSet.t -> G.vertex BatSet.t = fun toadd -> fun acc ->
    if(BatSet.cardinal toadd = 0) then 
      acc 
    else
      let next_toadd = BatSet.fold (fun vertex -> fun acc -> let pred = G.pred graph vertex in BatSet.union acc (BatSet.of_list pred)) toadd BatSet.empty in
      let new_acc = BatSet.union toadd acc in
      size_helper next_toadd new_acc  
  in
  let whole_set = size_helper olist_set BatSet.empty in
  BatSet.cardinal (BatSet.filter (fun vertex -> is_op_node vertex) whole_set)

let mult_size_of_graph : G.t -> int = fun graph ->
  let olist_set = BatSet.of_list (get_graph_ovlist graph) in
  let rec size_helper : G.vertex BatSet.t -> G.vertex BatSet.t -> G.vertex BatSet.t = fun toadd -> fun acc ->
    if(BatSet.cardinal toadd = 0) then 
      acc 
    else
      let next_toadd = BatSet.fold (fun vertex -> fun acc -> let pred = G.pred graph vertex in BatSet.union acc (BatSet.of_list pred)) toadd BatSet.empty in
      let new_acc = BatSet.union toadd acc in
      size_helper next_toadd new_acc  
  in
  let whole_set = size_helper olist_set BatSet.empty in
  BatSet.cardinal (BatSet.filter (fun vertex -> is_mult_node vertex) whole_set)


let rec get_bexp_of_node : G.t -> G.vertex -> bexp = fun graph -> fun node ->
  let is_valid_root = is_valid_node graph node in
  if(is_valid_root) then 
    match node with
    | (Node.CONST b, _) -> CONST b
    | (Node.VAR x, _) -> VAR x
    | (Node.AND, _) -> 
      let pred = G.pred graph node in
      let (b1_node, b2_node) = (List.nth pred 0, List.nth pred 1) in
      AND (get_bexp_of_node graph b1_node, get_bexp_of_node graph b2_node) 
    | (Node.OR, _) -> 
      let pred = G.pred graph node in
      let (b1_node, b2_node) = (List.nth pred 0, List.nth pred 1) in
      OR (get_bexp_of_node graph b1_node, get_bexp_of_node graph b2_node) 
    | (Node.XOR, _) -> 
      let pred = G.pred graph node in
      let (b1_node, b2_node) = (List.nth pred 0, List.nth pred 1) in
      XOR (get_bexp_of_node graph b1_node, get_bexp_of_node graph b2_node) 
    | (Node.NOT, _) -> 
      let pred = G.pred graph node in
      let b1_node= List.nth pred 0 in
      NOT (get_bexp_of_node graph b1_node) 
  else
    let _ = print_node node in
    let _ = print_newline() in
    let _ = print_string("num of pred : " ^ string_of_int(List.length (G.pred graph node))) in
    raise (Error ("getting bexp of invalid node"))


let rec get_vlist_expanded_bexp_of_node : G.t -> G.vertex -> var list -> bexp = fun graph -> fun node -> fun vlist ->
  let is_valid_root = is_valid_node graph node in
  if(is_valid_root) then 
    match node with
    | (Node.CONST b, _) -> CONST b
    | (Node.VAR x, 0) -> VAR x
    | (Node.VAR x, 1) -> 
      if (List.mem x vlist) then VAR x else
      let pred = G.pred graph node in
      let var_node = List.nth pred 0 in
      get_vlist_expanded_bexp_of_node graph var_node vlist
    | (Node.AND, _) -> 
      let pred = G.pred graph node in
      let (b1_node, b2_node) = (List.nth pred 0, List.nth pred 1) in
      AND (get_vlist_expanded_bexp_of_node graph b1_node vlist, get_vlist_expanded_bexp_of_node graph b2_node vlist) 
    | (Node.OR, _) -> 
      let pred = G.pred graph node in
      let (b1_node, b2_node) = (List.nth pred 0, List.nth pred 1) in
      OR (get_vlist_expanded_bexp_of_node graph b1_node vlist, get_vlist_expanded_bexp_of_node graph b2_node vlist) 
    | (Node.XOR, _) -> 
      let pred = G.pred graph node in
      let (b1_node, b2_node) = (List.nth pred 0, List.nth pred 1) in
      XOR (get_vlist_expanded_bexp_of_node graph b1_node vlist, get_vlist_expanded_bexp_of_node graph b2_node vlist) 
    | (Node.NOT, _) -> 
      let pred = G.pred graph node in
      let b1_node= List.nth pred 0 in
      NOT (get_vlist_expanded_bexp_of_node graph b1_node vlist) 
    | _ -> 
      let _ = print_node node in 
      let _ = print_newline() in
      let _ = print_string("num of pred : " ^ string_of_int(List.length (G.pred graph node))) in
      raise (Error ("expand invalid node"))
  else 
    let _ = print_node node in
    let _ = print_newline() in
    let _ = print_string("num of pred : " ^ string_of_int(List.length (G.pred graph node))) in
    raise (Error ("expand invalid node"))
 


let get_bexp_of_var : G.t -> var -> bexp = fun graph -> fun var ->
  let var_node = (Node.VAR var, 1) in
  let is_valid_root = is_valid_node graph var_node in
  let pred = if(is_valid_root) then List.nth (G.pred graph var_node) 0 else raise (Error ("invalid var node : " ^ var)) in
  get_bexp_of_node graph pred  
 

let print_graph : G.t -> unit = fun graph ->
  let tgt_vlist = get_graph_varlist graph in
  let _ =List.map (fun v -> print_string(v ^ " = "); Pp.print_bexp (get_bexp_of_var graph v);print_newline()) tgt_vlist
  in 
  ()

let rec get_expanded_bexp_of_node : G.t -> G.vertex -> bexp = fun graph -> fun node ->
  let is_valid_root = is_valid_node graph node in
  if(is_valid_root) then 
    match node with
    | (Node.CONST b, _) -> CONST b
    | (Node.VAR x, 0) -> VAR x
    | (Node.VAR x, 1) -> 
      let pred = G.pred graph node in
      let var_node = List.nth pred 0 in
      get_expanded_bexp_of_node graph var_node
    | (Node.AND, _) -> 
      let pred = G.pred graph node in
      let (b1_node, b2_node) = (List.nth pred 0, List.nth pred 1) in
      AND (get_expanded_bexp_of_node graph b1_node, get_expanded_bexp_of_node graph b2_node) 
    | (Node.OR, _) -> 
      let pred = G.pred graph node in
      let (b1_node, b2_node) = (List.nth pred 0, List.nth pred 1) in
      OR (get_expanded_bexp_of_node graph b1_node, get_expanded_bexp_of_node graph b2_node) 
    | (Node.XOR, _) -> 
      let pred = G.pred graph node in
      let (b1_node, b2_node) = (List.nth pred 0, List.nth pred 1) in
      XOR (get_expanded_bexp_of_node graph b1_node, get_expanded_bexp_of_node graph b2_node) 
    | (Node.NOT, _) -> 
      let pred = G.pred graph node in
      let b1_node= List.nth pred 0 in
      NOT (get_expanded_bexp_of_node graph b1_node) 
    | _ -> 
      let _ = print_node node in 
      let _ = print_newline() in
      let _ = print_string("num of pred : " ^ string_of_int(List.length (G.pred graph node))) in
      raise (Error ("expand invalid node"))
  else 
    let _ = print_node node in
    let _ = print_newline() in
    let _ = print_string("num of pred : " ^ string_of_int(List.length (G.pred graph node))) in
    let _ = print_node (List.nth (G.pred graph node) 0) in
    let _ = print_graph graph in
    raise (Error ("expand invalid node"))
 

let get_mult_depth_of_node : G.t -> G.vertex -> int = fun graph -> fun node ->
  let expanded_bexp = get_expanded_bexp_of_node graph node in
  get_mult_depth_expanded_bexp expanded_bexp 


let cleanup_graph_once : G.t -> G.vertex list -> (G.t * bool) = fun graph -> fun essential ->
  (*let _ = print_endline("essential node print") in
  let _ = List.map (fun v -> print_node v;print_newline ()) essential in
  let _ = print_newline() in*)
  G.fold_vertex 
  (fun v -> fun (acc_graph, acc_success) -> 
    (*let _ = print_node v; print_newline() in*)
    if(not (G.mem_vertex graph v)) then
      (acc_graph, acc_success)
    else if( ((G.succ graph v) = []) && not (List.mem v essential)) then 
      (*let _ = print_endline("node deleted") in*)
      let _ = print_string("dangling node deleted : ");print_node v;print_newline() in
      (G.remove_vertex acc_graph v, true) 
    else 
      (acc_graph, acc_success)
  )
  graph 
  (graph, false)

let rec cleanup_graph : G.t -> G.vertex list -> G.t = fun graph -> fun essential ->
  let (reduced_graph, is_reduced) = (cleanup_graph_once graph essential) in
  if(is_reduced) then cleanup_graph reduced_graph essential else reduced_graph
  

let detect_node_pair : G.t -> (G.vertex * G.vertex) -> G.t = fun graph -> fun (v1, v2) ->
  let ilist = get_graph_ilist graph in
  let olist = get_graph_olist graph in
  match (v1, v2) with
  | ((Node.CONST _, _), _) -> graph
  | (_, (Node.CONST _, _)) -> graph
  | ((Node.VAR var1, _), (Node.VAR var2, _)) ->
    if(List.mem var1 ilist || List.mem var2 ilist || List.mem var1 olist) then 
      graph
    else
      let b1 = get_bexp_of_var graph var1 in
      let b2 = get_bexp_of_var graph var2 in
      let is_same = is_equal_bexp b1 b2 in
      if(is_same) then
        let v1_succ = G.succ graph v1 in
        let new_graph_with_edge = List.fold_left (fun acc -> fun v -> (G.add_edge acc v2 v)) graph v1_succ in
        let new_graph_without_v1 = G.remove_vertex new_graph_with_edge v1 in
        let _ = print_string("redundant node deleted : ");print_node v1;print_string(" is same with ");print_node v2;print_newline() in
        new_graph_without_v1
      else
        graph
    | _ -> raise (Error "detecting non-var node")


let rec detect_one_level : G.t -> G.vertex list -> G.t = fun graph -> fun todo ->
  match todo with
  | [] ->  graph
  | hd::tl -> 
    let detect_hd_graph = 
      List.fold_left 
        (fun acc -> fun v -> 
        if(G.mem_vertex acc hd) then detect_node_pair acc (hd, v) else acc) graph tl 
    in
    detect_one_level detect_hd_graph tl


let rec get_next_level : G.t -> G.vertex BatSet.t -> G.vertex BatSet.t -> G.vertex BatSet.t = fun graph -> fun detected_set -> fun current_level ->
  let ilist = get_graph_ilist graph in
  let current_vlist = BatSet.to_list current_level in
  let succ_once_set = BatSet.of_list (List.flatten (List.map (fun v -> (G.succ graph v)) current_vlist)) in
  (*let _ = print_endline("succ_once set print") in
  let _ = BatSet.map (fun v -> print_node v; print_newline()) succ_once_set in
  let _ = print_endline("succ of each nodes print") in
  let _ = BatSet.map (fun v -> (List.map (fun x -> (print_node x; print_newline())) (G.succ graph v))) current_level in  *)
  let rec succ_helper : G.vertex BatSet.t -> G.vertex BatSet.t = fun current_level ->
    let current_vlist = BatSet.to_list current_level in
    let succ_once = 
      BatSet.of_list (
        List.flatten (
          List.map 
          (fun v -> 
          if(is_var_node v) then
            let b_v = get_bexp_of_var graph (get_var_of_node v) in
            let varlist = get_bexp_varlist b_v in
            let vlist = List.map (fun var -> if(List.mem var ilist) then (Node.VAR var, 0) else (Node.VAR var, 1)) varlist in
            let detect_filter = List.filter (fun v -> not (BatSet.mem v detected_set)) vlist in
            if(detect_filter = []) then [v] else []
          else 
            G.succ graph v 
          )
          current_vlist
        )
      ) 
    in
    (*let _ = print_endline("current_level set print") in
    let _ = BatSet.map (fun v -> print_node v; print_newline()) current_level in
    let _ = print_endline("succ_once set print") in
    let _ = BatSet.map (fun v -> print_node v; print_newline()) succ_once in
    *)
    if(BatSet.equal succ_once current_level) then succ_once else succ_helper succ_once
  in
  succ_helper succ_once_set

let redundancy_opt : G.t -> G.t = fun graph -> 
  let ilist = get_graph_ilist graph in
  let ilist_vertex = List.map (fun var -> (Node.VAR var, 0)) ilist in
  let level_zero = BatSet.of_list ((Node.CONST true, 0)::(Node.CONST false, 0)::ilist_vertex) in
  let rec opt_helper : G.t -> G.vertex BatSet.t -> G.vertex BatSet.t -> G.t = fun graph -> fun detected_set -> fun current_level ->
    if(BatSet.is_empty current_level) then 
      graph
    else
      let opt_graph = detect_one_level graph (BatSet.to_list current_level) in
      (*let _ = print_endline("one level detection finished") in*)
      let new_detected_set = BatSet.union detected_set current_level in
      let next_level = get_next_level graph new_detected_set current_level in
      (*let _ = print_endline("detected node set print") in
      let _ = BatSet.map (fun v -> print_node v; print_newline()) new_detected_set in*)
      (*let _ = print_endline("next level node set print") in
      let _ = BatSet.map (fun v -> print_node v; print_newline()) next_level in*)
      
      (*let _ = print_endline("getting next level finished") in*)
      opt_helper opt_graph new_detected_set next_level
  in
  opt_helper graph BatSet.empty level_zero



