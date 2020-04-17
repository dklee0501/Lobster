open Circuit

type depth = int
type level = int
type subinput = var
type suboutput = var
(* WLOG, assume 1 output *)
type subregion = subinput list * suboutput list * eqn list

(* l2 - l1 *)
let list_erase l1 l2 = List.filter (fun x -> not (List.mem x l1)) l2

let rec list_merge l1 l2 =
  match l1 with
  | [] -> l2
  | hd::tl -> if(List.mem hd l2) then list_merge tl l2 else list_merge tl (hd::l2)

let rec list_subset l1 l2 = 
  match l1 with
  | [] -> true
  | hd::tl -> if(List.mem hd l2) then list_subset tl l2 else false

let list_max l1 =
  let rec max_helper l1 acc =
    match l1 with
    | [] -> acc
    | hd::tl -> if (acc < hd) then max_helper tl hd else max_helper tl acc
  in
  max_helper l1 0

let merge_subregion : subregion -> subregion -> subregion = fun sub1 -> fun sub2 ->
  let (ilist1, olist1, elist1) = sub1 in
  let (ilist2, olist2, elist2) = sub2 in
  let (newi, newo, newe) = (list_merge ilist1 ilist2, list_merge olist1 olist2, list_merge elist1 elist2) in
  (List.filter (fun x -> not(List.mem x newo)) newi, newo, newe)


let expand_subregion_to_elist : subregion -> suboutput -> eqn list = fun sub -> fun sout ->
  let (silist, solist, selist) = sub in
  (*let eqnlist = List.rev eqnlist in*)
  let rec expand_helper : eqn list -> var list -> eqn list = fun acc -> fun toadd ->
    match toadd with
    | [] -> acc
    | hd::tl ->
      if(List.mem hd silist) then 
        expand_helper acc tl
      else
        let hd_bexp = get_rhs_of_var selist hd in
        let hd_eqn = (hd, hd_bexp) in
         match hd_bexp with
         | CONST b -> expand_helper (hd_eqn::acc) tl
         | VAR v -> expand_helper (hd_eqn::acc) (v::tl)
         | AND (b1, b2) | OR(b1, b2) | XOR(b1, b2) -> 
           let vlist = list_merge (get_bexp_varlist b1) (get_bexp_varlist b2) in 
           expand_helper (hd_eqn::acc) (list_merge vlist tl)
         | NOT b1-> expand_helper (hd_eqn::acc) (list_merge (get_bexp_varlist b1) tl)
         | NULL -> raise (Error "NULL var is used")
  in
  expand_helper [] (sout::[])



let is_valid_output_of_subregion : circuit -> var -> subregion -> bool = fun cir -> fun v -> fun sub ->
  let (ilist, olist, elist) = cir in
  let (silist, solist, selist) = sub in
  let usepoints = get_usepoint_of_var elist v in
  (List.mem v olist) || not(list_subset usepoints selist)

(* subinput을 output으로 하는 작은 subregion *)
let fanin_subinput : circuit -> subinput -> subregion = fun cir -> fun sub ->
  let (ilist, olist, elist) = cir in
  if(List.mem sub ilist) then
    (sub::[], [], [])
  else
    let subinput_eqn = (sub, get_rhs_of_var elist sub) in
    let used_varlist = get_eqn_rhs_varlist subinput_eqn in
    (used_varlist, sub::[], subinput_eqn::[])



let fanin_subregion : circuit -> subregion -> subregion = fun cir -> fun sub ->
  let (ilist, olist, elist) = sub in
  let merge_ready_sub = ([], olist, elist) in
  let subregions_to_merge = List.map (fanin_subinput cir) ilist in
  let new_sub_tmp = List.fold_left (fun acc -> fun sub_to_add -> (merge_subregion acc sub_to_add)) merge_ready_sub subregions_to_merge in
  let (new_ilist, new_olist, new_elist) = new_sub_tmp in
  (new_ilist, (List.filter (fun x -> is_valid_output_of_subregion cir x new_sub_tmp) new_olist), new_elist)

(* subinput 중 가장 높은 depth의 input으로만 fanin *)
let fanin_subregion_maxdepth_input : circuit -> subregion -> subregion = fun cir -> fun sub ->
  let vdmap = get_var_depth_map cir in
  let (ilist, olist, elist) = sub in
  let ilist_to_fanin = get_max_depth_varlist ilist vdmap in
  let merge_ready_sub = (list_erase ilist_to_fanin ilist, olist, elist) in
  let subregions_to_merge = List.map (fanin_subinput cir) ilist_to_fanin in
  let new_sub_tmp = List.fold_left (fun acc -> fun sub_to_add -> (merge_subregion acc sub_to_add)) merge_ready_sub subregions_to_merge in
  let (new_ilist, new_olist, new_elist) = new_sub_tmp in
  (new_ilist, (List.filter (fun x -> is_valid_output_of_subregion cir x new_sub_tmp) new_olist), new_elist)


let select_subregion : circuit -> var -> level -> subregion = fun cir -> fun root -> fun level ->
  let (ilist, olist, elist) = cir in
  let initial_subregion = 
    if (List.mem root ilist) then 
      (root::[], [], []) 
    else 
      ( (get_bexp_varlist (get_rhs_of_var elist root)), root::[], (root, (get_rhs_of_var elist root))::[] ) in
  let rec fanin_helper : subregion -> level -> int -> subregion = fun sub -> fun level -> fun max_input_num ->
    if (level = 0) then
      sub
    else
      (* fanin방식 바꾸려면 여기에 쓰이는 함수를 수정 *)
      let new_sub = fanin_helper (fanin_subregion_maxdepth_input cir sub) (level - 1) max_input_num in
      let (new_silist, _, _) = new_sub in
      if(List.length new_silist > max_input_num) then
        sub
      else
        new_sub
  in
  (* max_input_num *)
  fanin_helper initial_subregion level 4


(*

let select_subregion : circuit -> var -> level -> subregion = fun cir -> fun root -> fun level ->
  let (ilist, olist, elist) = cir in
  let initial_subregion = 
    if (List.mem root ilist) then 
      (root::[], [], []) 
    else 
      ( (get_bexp_varlist (get_rhs_of_var elist root)), root::[], (root, (get_rhs_of_var elist root))::[] ) in
  let rec fanin_helper : subregion -> level -> int -> subregion = fun sub -> fun level -> fun max_input_num ->
    let (silist, _, _) = sub in
    if (level = 0 || List.length silist > max_input_num) then
      sub
    else
      (* fanin방식 바꾸려면 여기에 쓰이는 함수를 수정 *)
      fanin_helper (fanin_subregion_maxdepth_input cir sub) (level - 1) max_input_num
  in
  (* max_input_num *)
  fanin_helper initial_subregion level 7

   
let select_random_subregion : circuit -> level -> subregion = fun cir -> fun level ->
  let (ilist, olist, elist) = cir in
  let candidate_root = olist @ (List.map fst elist) in
  let _ = Random.self_init() in
  let random_index = Random.int (List.length candidate_root) in
  let root = List.nth candidate_root random_index in
  let _ = print_endline("select ramdom subregion start") in
  let _ = print_endline("root node : " ^ root) in
  let _ = print_endline("fanin level : " ^ (string_of_int level)) in
  select_subregion cir root level

let select_maxdepth_subregion : circuit -> level -> subregion = fun cir -> fun level ->
  let root = fst (get_max_mult_depth cir) in
  let _ = print_endline("select max depth subregion start") in
  let _ = print_endline("root node : " ^ root) in
  let _ = print_endline("fanin level : " ^ (string_of_int level)) in
  select_subregion cir root level

let select_maxdiff_subregion : circuit -> level -> subregion = fun cir -> fun level ->
  let root = fst (get_max_mult_depth_diff cir) in
  let _ = print_endline("select max depth diff subregion start") in
  let _ = print_endline("root node : " ^ root) in
  let _ = print_endline("fanin level : " ^ (string_of_int level)) in
  select_subregion cir root level


let rec print_varlist : var list -> unit = fun varlist ->
  match varlist with
  | [] -> print_newline()
  | var::tl -> let _ = print_string(var ^ " ") in print_varlist tl

let select_critical_subregion : circuit -> level -> subregion = fun cir -> fun level ->
  let critical_path = find_oneof_critical_path cir in
  let _ = print_string("selected critical path : ");print_varlist critical_path in
  let select_index = let _ = Random.self_init() in Random.int (List.length critical_path) in
  let root = List.nth critical_path select_index in
  let _ = print_endline("select critical subregion start") in
  let _ = print_endline("root node : " ^ root) in
  let _ = print_endline("fanin level : " ^ (string_of_int level)) in
  select_subregion cir root level
*)
let select_subregion_from_root : circuit -> level -> var -> subregion = fun cir -> fun level -> fun root ->
  let _ = print_endline("select subregion from root start") in
  let _ = print_endline("root node : " ^ root) in
  let _ = print_endline("fanin level : " ^ (string_of_int level)) in
  select_subregion cir root level

let trim_elist cir elist =
  let vdmap = get_var_depth_map cir in
  let rec trim_helper defined_vlist acc elist =
    match elist with
    | [] -> acc
    | hd::tl -> 
      if (List.mem (fst hd) defined_vlist) then
        let expanded_hd = expand cir (snd hd) in
        let hd_old_depth = find_depth vdmap (fst hd) in
        let hd_new_depth = get_mult_depth_expanded_bexp expanded_hd in 
        if(hd_new_depth < hd_old_depth) then
	  trim_helper defined_vlist (hd::(List.filter (fun (v,bexp) -> not(v = (fst hd))) acc)) tl
        else
          trim_helper defined_vlist acc tl 
      else 
        trim_helper ((fst hd)::defined_vlist) (hd::acc) tl
  in
  trim_helper [] [] elist

(* oldsub, newsub in/out 인터페이스가 동일할때 circuit을 재조립*)
let replace_subregion : circuit -> subregion -> subregion -> circuit = fun cir -> fun oldsub -> fun newsub ->
  let (old_ilist, old_olist, old_elist) = oldsub in
  let (new_ilist, new_olist, new_elist) = newsub in
  let valid_new_ilist = list_subset new_ilist old_ilist in
  let valid_new_olist = list_subset old_olist new_olist in
  let valid_new_sub = (valid_new_ilist && valid_new_olist) in
  let _ = if(not(valid_new_sub)) then raise (Error "invalid subregion replace") else 1 in
  let (ilist, olist, elist) = cir in
  let newcircuit_elist = new_elist @ (list_erase old_elist elist) in
  let newcircuit_elist_trim = trim_elist cir newcircuit_elist in
  (ilist, olist, newcircuit_elist_trim)
  



