
type var = string
type bexp = NULL | CONST of bool
	  | VAR of var 
	  | AND of (bexp * bexp)
	  | XOR of (bexp * bexp)
	  | OR of (bexp * bexp)
	  | NOT of bexp
type eqn = var * bexp
type input = var
type output = var
type circuit = input list * output list * eqn list
(* var, depth, depth_diff map *)
(*type depth_map = (var * int * int) list*)
type depth_map = (var * int) list


exception Error of string

(* get used var list - input, output, lexps *)
let get_varlist : circuit -> var list = fun cir ->
  let (ilist, olist, eqnlist) = cir in
  ilist @ (List.map fst eqnlist)

let rec list_merge l1 l2 =
  match l1 with
  | [] -> l2
  | hd::tl -> if(List.mem hd l2) then list_merge tl l2 else list_merge tl (hd::l2)

let rec list_inter l1 l2 =
  match l1 with
  | [] -> []
  | hd::tl -> if (List.mem hd l2) then hd::(list_inter tl l2) else list_inter tl l2

let rec list_subset l1 l2 =
  match l1 with
  | [] -> true
  | hd::tl -> (List.mem hd l2) && (list_subset tl l2)

let rec get_bexp_size : bexp -> int = fun bexp ->
  match bexp with
  | CONST b -> 1
  | VAR v -> 1
  | NOT b1 -> 1 + (get_bexp_size b1)
  | AND(b1, b2) | XOR(b1, b2) | OR(b1, b2) -> 1 + (get_bexp_size b1) + (get_bexp_size b2)
  | NULL -> raise (Error "NULL var is used")

let rec get_bexp_varlist : bexp -> var list = fun bexp ->
  match bexp with
  | CONST b -> []
  | VAR v -> v::[]
  | NOT b1 -> get_bexp_varlist b1
  | AND(b1, b2) | XOR(b1, b2) | OR(b1, b2) -> list_merge (get_bexp_varlist b1) (get_bexp_varlist b2)
  | NULL -> raise (Error "NULL var is used")

let get_eqn_rhs_varlist : eqn -> var list = fun eqn ->
  get_bexp_varlist (snd eqn)

let rec get_rhs_of_var : eqn list -> var -> bexp = fun elist -> fun v ->
  match elist with
  | [] -> raise (Error "undefined var in eqn list")
  | hd::tl -> if ((fst hd) = v) then snd hd else get_rhs_of_var tl v


let get_usepoint_of_var : eqn list -> var -> eqn list = fun elist -> fun v ->
  let usepoint_check : var -> eqn -> bool = fun v -> fun e -> 
    let vlist = get_eqn_rhs_varlist e in
    List.mem v vlist
  in
  let result = List.filter (usepoint_check v) elist in
  result


let rec find_depth : depth_map -> var -> int = fun vdmap -> fun v ->
  match vdmap with
  | [] -> raise (Error ("Undefined variable" ^ v ^ " in depth map"))
  | (v1, d1)::tl -> if(v = v1) then d1 else find_depth tl v
(*
let rec find_depth_diff : depth_map -> var -> int = fun vdmap -> fun v ->
  match vdmap with
  | [] -> raise (Error "Undefined variable in depth diff map")
  | (v1, d1, df1)::tl -> if(v = v1) then df1 else find_depth tl v
*)

let rec get_bexp : eqn list -> var -> bexp = fun elist -> fun v ->
  match elist with
  | [] -> raise (Error ("undefined var " ^ v ^ " in eqn list"))
  | hd::tl -> if ((fst hd) = v) then snd hd else get_bexp tl v


let rec expand : circuit -> bexp -> bexp = fun cir -> fun bexp ->
  let (ilist, olist, elist) = cir in
  let rec expand_bexp : bexp -> bexp =  fun bexp ->
    match bexp with
    | CONST b -> CONST b
    | VAR v -> 
      if(List.mem v ilist) then
        VAR v
      else
        let v_bexp = (get_bexp elist v) in
        expand_bexp v_bexp
    | AND (b1, b2) -> AND(expand_bexp b1, expand_bexp b2)
    | OR (b1, b2) -> OR(expand_bexp b1, expand_bexp b2)
    | XOR (b1, b2) -> XOR(expand_bexp b1, expand_bexp b2)
    | NOT b1-> NOT(expand_bexp b1)
    | NULL -> raise (Error "NULL var is used")
  in
  expand_bexp bexp


let rec get_mult_depth_expanded_bexp : bexp -> int = fun bexp ->
  match bexp with
  | CONST b -> 0
  | VAR v -> 0
  | AND (b1, b2) -> 
    let d1 = get_mult_depth_expanded_bexp b1 in
    let d2 = get_mult_depth_expanded_bexp b2 in
    if(d1 < d2) then d2 + 1 else d1 + 1
  | XOR (b1, b2) -> 
    let d1 = get_mult_depth_expanded_bexp b1 in
    let d2 = get_mult_depth_expanded_bexp b2 in
    if(d1 < d2) then d2 else d1
  | OR (b1, b2) -> 
    let d1 = get_mult_depth_expanded_bexp b1 in
    let d2 = get_mult_depth_expanded_bexp b2 in
    if(d1 < d2) then d2 + 1 else d1 + 1
  | NOT b1 -> get_mult_depth_expanded_bexp b1
  | NULL -> raise (Error "NULL var is used")

let get_mult_depth_bexp : circuit -> bexp -> int = fun cir -> fun bexp ->
  let expanded_bexp = expand cir bexp in
  get_mult_depth_expanded_bexp expanded_bexp

(*
let rec get_mult_depth_diff_expanded_bexp : bexp -> int =  fun bexp ->
  match bexp with
  | CONST b -> 0 | VAR v -> 0 | NOT b1 -> 0
  | AND (b1, b2) | XOR(b1, b2) | OR(b1, b2) -> 
    let diff = (get_mult_depth_expanded_bexp b1) - (get_mult_depth_expanded_bexp b2) in
    if (diff > 0) then diff else 0 - diff
  | NULL -> raise (Error "NULL var is used")
*)


let rec get_mult_depth_bexp_with_vdmap : (var, int) BatMap.t -> bexp -> int = fun vdmap -> fun bexp ->
  match bexp with
  | NULL -> raise (Error "NULL var is used")
  | CONST b -> 0
  | VAR v -> BatMap.find v vdmap 
  | AND (b1, b2) -> 
    let d1 = get_mult_depth_bexp_with_vdmap vdmap b1 in
    let d2 = get_mult_depth_bexp_with_vdmap vdmap b2 in
    if(d1 < d2) then d2 + 1 else d1 + 1
  | OR (b1, b2) -> 
    let d1 = get_mult_depth_bexp_with_vdmap vdmap b1 in
    let d2 = get_mult_depth_bexp_with_vdmap vdmap b2 in
    if(d1 < d2) then d2 + 1 else d1 + 1
  | XOR (b1, b2) -> 
    let d1 = get_mult_depth_bexp_with_vdmap  vdmap b1 in
    let d2 = get_mult_depth_bexp_with_vdmap  vdmap b2 in
    if(d1 < d2) then d2 else d1
  | NOT b1 -> get_mult_depth_bexp_with_vdmap  vdmap b1

(*
let rec get_mult_depth_diff_bexp : depth_map -> bexp -> int = fun vdmap -> fun bexp ->
  match bexp with
  | CONST b -> 0 | VAR v -> 0 | NOT b1 -> 0
  | AND (b1, b2) | XOR(b1, b2) | OR(b1, b2) -> 
    let diff = (get_mult_depth_bexp vdmap b1) - (get_mult_depth_bexp vdmap b2) in
    if (diff > 0) then diff else 0 - diff
*)

(* TODO : use Map functor *)
let get_var_depth_map : circuit -> depth_map = fun cir ->
  let (ilist, olist, elist) = cir in
  let ilist_depth_map = List.map (fun x -> (x, 0)) ilist in
  let rec map_helper : eqn list -> depth_map -> depth_map = fun elist -> fun acc ->
    match elist with
    | [] -> acc
    | (v, bexp)::tl ->
      let bexp = expand cir bexp in
      let bexp_depth = get_mult_depth_expanded_bexp bexp in
      (*let bexp_depth_diff = get_mult_depth_diff_expanded_bexp bexp in*)
      map_helper tl ((v, bexp_depth)::acc)
  in
  map_helper elist ilist_depth_map

let get_var_mult_depth : circuit -> var -> int = fun cir -> fun v ->
  let var_depth_map = get_var_depth_map cir in
  find_depth var_depth_map v
(*
let get_var_mult_depth_diff : circuit -> var -> int = fun cir -> fun v ->
  let var_depth_map = get_var_depth_map cir in
  find_depth_diff var_depth_map v
*)

let max_depth : depth_map -> (var * int) = fun vdmap ->
  let rec max_depth_helper vdmap acc = 
    match vdmap with
    | [] -> acc
    | (v1, d1)::tl -> if((snd acc) <= d1) then max_depth_helper tl (v1, d1) else max_depth_helper tl acc
  in
  max_depth_helper vdmap ("d0", 0)

(*
let max_depth_diff : depth_map -> (var * int) = fun vdmap ->
  let rec max_depth_diff_helper vdmap acc = 
    match vdmap with
    | [] -> acc
    | (v1, d1, df1)::tl -> if((snd acc) <= df1) then max_depth_diff_helper tl (v1, df1) else max_depth_diff_helper tl acc
  in
  max_depth_diff_helper vdmap ("df0", 0)
*)

let get_max_mult_depth : circuit -> (var * int) = fun cir ->
  let var_depth_map = get_var_depth_map cir in
  max_depth var_depth_map

(*
let get_max_mult_depth_diff : circuit -> (var * int) = fun cir ->
  let var_depth_map = get_var_depth_map cir in
  max_depth_diff var_depth_map
*)

let rec get_max_depth_varlist : var list -> depth_map -> var list = fun vlist -> fun vdmap ->
  let rec helper vlist vdmap acc max = 
   match vlist with
   | [] -> acc
   | hd::tl -> let hd_depth = find_depth vdmap hd in  
                 if(hd_depth < max) then helper tl vdmap acc max 
                 else if (hd_depth = max) then helper tl vdmap (hd::acc) max
                 else helper tl vdmap [hd] hd_depth 
  in
  helper vlist vdmap [] 0


let find_critical_parents : circuit -> var -> var list = fun cir -> fun v ->
  let (ilist, olist, elist) = cir in
  if(List.mem v ilist) then 
    []
  else
    let vdmap = get_var_depth_map cir in
    let v_bexp = get_rhs_of_var elist v in
    let parents_varlist = get_bexp_varlist v_bexp in
    get_max_depth_varlist parents_varlist vdmap

let find_oneof_critical_path : circuit -> var list = fun cir ->
  let root = fst( get_max_mult_depth cir ) in
  let rec critical_helper cir var acc =
    let parents = find_critical_parents cir var in
    match parents with
    | [] -> acc
    | _ ->
      let select_index = let _ = Random.self_init() in Random.int (List.length parents) in
      let selected_parent = List.nth parents select_index in
      critical_helper cir selected_parent (var::acc)
  in
  critical_helper cir root []




let pattern_check : circuit -> bexp -> bexp -> bool = fun cir -> fun pattern -> fun tgt ->
  let (ilist, olist, elist) = cir in
  let rec helper : bexp -> bexp -> bool = fun pattern -> fun tgt ->
   match pattern with
    | NULL -> raise (Error "NULL var is used")
    | CONST b -> (tgt = pattern)
    | VAR v -> true 
    | NOT pattern' -> 
      (match tgt with
      | NULL -> raise (Error "NULL var is used")
      | VAR x -> if(List.mem x ilist) then false else let tgt' = get_bexp elist x in helper pattern tgt'
      | NOT tgt' -> helper pattern' tgt' 
      | _ -> false)
    | AND (pattern1, pattern2) ->
      (match tgt with
      | NULL -> raise (Error "NULL var is used")
      | VAR x -> if(List.mem x ilist) then false else let tgt' = get_bexp elist x in helper pattern tgt'
      | AND (tgt1, tgt2) -> ((helper pattern1 tgt1) && (helper pattern2 tgt2)) || ((helper pattern1 tgt2) && (helper pattern2 tgt1))
      | _ -> false)
    | OR (pattern1, pattern2) ->
      (match tgt with
      | NULL -> raise (Error "NULL var is used")
      | VAR x -> if(List.mem x ilist) then false else let tgt' = get_bexp elist x in helper pattern tgt'
      | OR (tgt1, tgt2) -> ((helper pattern1 tgt1) && (helper pattern2 tgt2)) || ((helper pattern1 tgt2) && (helper pattern2 tgt1))
      | _ -> false)
    | XOR (pattern1, pattern2) ->
      (match tgt with
      | NULL -> raise (Error "NULL var is used")
      | VAR x -> if(List.mem x ilist) then false else let tgt' = get_bexp elist x in helper pattern tgt'
      | XOR (tgt1, tgt2) -> ((helper pattern1 tgt1) && (helper pattern2 tgt2)) || ((helper pattern1 tgt2) && (helper pattern2 tgt1))
      | _ -> false)
  in
  helper pattern tgt

let pattern_count : circuit -> bexp -> int = fun cir -> fun pattern -> 
  let (ilist, olist, elist) = cir in
  let matched_list = List.filter (fun e -> pattern_check cir pattern  (snd e)) elist in
  List.length matched_list
 



