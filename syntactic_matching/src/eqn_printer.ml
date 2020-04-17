open Circuit
open Graph_circuit

let var_index = ref 0

let new_var : unit -> string = fun unit -> 
  let new_index = !var_index in 
  let _ = var_index := !var_index + 1 in
  "tmp" ^ (string_of_int new_index)

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
    else if(is_terminal b1) then
      let b2_lv = new_var() in
      let b2_eqnlist = eqn_to_single_gate (b2_lv, b2) in
      b2_eqnlist @ [(lv, (AND (b1, (VAR b2_lv))))]
    else if(is_terminal b2) then
      let b1_lv = new_var() in
      let b1_eqnlist = eqn_to_single_gate (b1_lv, b1) in
      b1_eqnlist @ [(lv, (AND ((VAR b1_lv), b2)))]
    else
      let b1_lv = new_var() in
      let b1_eqnlist = eqn_to_single_gate (b1_lv, b1) in
      let b2_lv = new_var() in
      let b2_eqnlist = eqn_to_single_gate (b2_lv, b2) in
      b1_eqnlist @ b2_eqnlist @ [(lv, (AND ((VAR b1_lv), (VAR b2_lv))))]
  | XOR (b1, b2) ->
    if(is_terminal b1 && is_terminal b2) then 
      [eqn] 
    else if(is_terminal b1) then
      let b2_lv = new_var() in
      let b2_eqnlist = eqn_to_single_gate (b2_lv, b2) in
      b2_eqnlist @ [(lv, (XOR (b1, (VAR b2_lv))))]
    else if(is_terminal b2) then
      let b1_lv = new_var() in
      let b1_eqnlist = eqn_to_single_gate (b1_lv, b1) in
      b1_eqnlist @ [(lv, (XOR ((VAR b1_lv), b2)))]
    else
      let b1_lv = new_var() in
      let b1_eqnlist = eqn_to_single_gate (b1_lv, b1) in
      let b2_lv = new_var() in
      let b2_eqnlist = eqn_to_single_gate (b2_lv, b2) in
      b1_eqnlist @ b2_eqnlist @ [(lv, (XOR ((VAR b1_lv), (VAR b2_lv))))]
  | OR (b1, b2) -> 
    if(is_terminal b1 && is_terminal b2) then 
      [eqn] 
    else if(is_terminal b1) then
      let b2_lv = new_var() in
      let b2_eqnlist = eqn_to_single_gate (b2_lv, b2) in
      b2_eqnlist @ [(lv, (OR (b1, (VAR b2_lv))))]
    else if(is_terminal b2) then
      let b1_lv = new_var() in
      let b1_eqnlist = eqn_to_single_gate (b1_lv, b1) in
      b1_eqnlist @ [(lv, (OR ((VAR b1_lv), b2)))]
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



let rec bexp_to_string : bexp -> string = fun bexp ->
  match bexp with
  | CONST b -> if(b) then "true" else "false" 
  | VAR v -> v
  | AND (b1, b2) -> "(" ^ bexp_to_string b1 ^ " * " ^ bexp_to_string b2 ^ ")"
  | XOR (b1, b2) -> "(!" ^ bexp_to_string b1 ^ " * " ^ bexp_to_string b2 ^ ") + (" ^ bexp_to_string b1 ^ " * !" ^ bexp_to_string b2 ^ ")"
  | OR (b1, b2) -> "(" ^ bexp_to_string b1 ^ " + " ^ bexp_to_string b2 ^ ")"
  | NOT b1 -> "(!" ^ bexp_to_string b1 ^ ")"
  | NULL -> raise (Error "NULL var is used")


let eqn_to_string : eqn -> string = fun eqn ->
  let (lexp, bexp) = eqn in
  let str1 = lexp ^ " = " in
  let str2 = bexp_to_string bexp in
  str1 ^ str2 ^ ";\n"

let rec elist_to_string : eqn list -> string = fun elist ->
  match elist with
  | [] -> "\n"
  | e::tl -> (eqn_to_string e) ^ (elist_to_string tl)


let graph_to_eqnstring : G.t -> string = fun graph ->
  let _ = var_index := 0 in
  let ilist = get_graph_ilist graph in
  let ilist_vertex = List.map (fun i -> (Node.VAR i, 0)) ilist in
  let olist = get_graph_olist graph in
  let ilist_string = (List.fold_left (fun acc -> fun var -> acc ^ " " ^ var) "INORDER =" ilist) ^ ";\n" in
  let olist_string = (List.fold_left (fun acc -> fun var -> acc ^ " " ^ var) "OUTORDER =" olist) ^ ";\n" in
  let rec eqnstring_order : var list -> (G.vertex list * G.vertex BatSet.t) -> var list = 
  fun acc -> fun (todo, delayed_set) ->   
    if(todo = []) then 
     if(BatSet.is_empty delayed_set) then acc else eqnstring_order acc (BatSet.to_list delayed_set, BatSet.empty)
    else
      let (new_acc, new_delayed) =
        List.fold_left
          (fun (acc, acc_delayed) -> fun vertex -> 
             match fst vertex with 
             | Node.CONST b -> (acc, acc_delayed)
             | Node.AND | Node.OR | Node.XOR | Node.NOT -> (acc, acc_delayed)
             | Node.VAR x -> 
               if(List.mem x ilist || List.mem x acc) then
                 (acc, acc_delayed)
               else
                 let bexp = get_bexp_of_var graph x in 
                 let bexp_varlist = get_bexp_varlist bexp in
                 let delay_check = List.filter (fun var -> not(List.mem var (ilist @ acc))) bexp_varlist in
                 if(delay_check = []) then
                   (acc @ [x], acc_delayed)
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
      eqnstring_order new_acc (new_todo, new_delayed)
  in
  let start_vlist = (Node.CONST true, 0)::(Node.CONST false, 0)::ilist_vertex in
  let eqn_order = eqnstring_order [] (start_vlist, BatSet.empty) in
  (*let _ = Pp.print_varlist eqn_order in*)
  let eqn_list = List.map (fun lv -> (lv, get_bexp_of_var graph lv)) eqn_order in
  let single_eqn_list = List.flatten (List.map eqn_to_single_gate eqn_list) in
  let elist_string = elist_to_string single_eqn_list in 
  ilist_string ^ olist_string ^ elist_string


let rec simplize_bexp : bexp -> var list -> bexp = fun bexp -> fun cripath ->
  match bexp with
  | CONST b -> bexp
  | VAR v -> if(List.mem v cripath) then bexp else CONST true 
  | AND (b1, b2) -> AND (simplize_bexp b1 cripath, simplize_bexp b2 cripath)
  | XOR (b1, b2) -> XOR (simplize_bexp b1 cripath, simplize_bexp b2 cripath)
  | OR (b1, b2) -> OR (simplize_bexp b1 cripath, simplize_bexp b2 cripath)
  | NOT b1 -> NOT (simplize_bexp b1 cripath)
  | NULL -> raise (Error "NULL var is used")


let graph_to_critical_string : G.t -> var list -> string = fun graph -> fun cripath ->
  let _ = var_index := 0 in
  let ilist = get_graph_ilist graph in
  (*let ilist_vertex = List.map (fun i -> (Node.VAR i, 0)) ilist in*)
  let olist = get_graph_olist graph in
  (*from input to output*)
  let eqn_order = List.filter (fun x -> not (List.mem x ilist)) cripath in
  let cri_input_lv = (List.hd eqn_order) in
  let cri_input_eqn = get_bexp_of_var graph cri_input_lv in
  let critical_ilist = get_bexp_varlist cri_input_eqn in
  let eqn_list = List.map (fun lv -> (lv, simplize_bexp (get_bexp_of_var graph lv) (critical_ilist @ cripath)) ) eqn_order in
  let single_eqn_list = List.flatten (List.map eqn_to_single_gate eqn_list) in
  let critical_olist = List.filter (fun x -> List.mem x cripath) olist in
  let ilist_string = (List.fold_left (fun acc -> fun var -> acc ^ " " ^ var) "INORDER =" critical_ilist) ^ ";\n" in
  let olist_string = (List.fold_left (fun acc -> fun var -> acc ^ " " ^ var) "OUTORDER =" critical_olist) ^ ";\n" in
  let elist_string = elist_to_string single_eqn_list in 
  ilist_string ^ olist_string ^ elist_string


let printf_string : string -> string -> unit = fun filename -> fun eqnstring ->
  let channel_out = open_out filename in
  let _ = output_string channel_out eqnstring in
  let _ = close_out channel_out in
  ()

let printf_graph : string -> G.t -> unit = fun filename -> fun graph ->
  let eqnstring = graph_to_eqnstring graph in 
  printf_string filename eqnstring

let print_graph :  G.t -> unit = fun graph ->
  let eqnstring = graph_to_eqnstring graph in 
  print_endline eqnstring
