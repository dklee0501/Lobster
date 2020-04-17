open Circuit

let z3_string : bexp -> bexp -> string = fun b1 -> fun b2 ->
  let varlist = get_bexp_varlist b1 in
  let var2declare var = "(declare-const " ^ var ^ " Bool)\n" in
  let declare_string = List.fold_left (fun acc -> fun var -> acc ^ (var2declare var)) "" varlist in
  let fun_string = 
"(define-fun is_equal () Bool
	(= " ^ (Pp.bexp_to_string b1) ^ " " ^ (Pp.bexp_to_string b2) ^ ")
)\n"
  in
  let assert_string = 
"(assert (not is_equal))
(check-sat)" 
  in
  declare_string ^ fun_string ^ assert_string

let z3_print : string -> bexp -> bexp -> unit = fun dirname -> fun b1 -> fun b2 ->
  let to_z3_string = z3_string b1 b2 in
  let file = open_out ("./z3_tmpdir/" ^ dirname ^ "z3_input.tmp") in
  let _ = output_string file to_z3_string in
  let _ = close_out file in
  ()
  
let execute_z3 : string -> bool = fun dirname ->
  let z3_inputpath = "./z3_tmpdir/" ^ dirname ^ "z3_input.tmp" in
  let z3_outputpath = "./z3_tmpdir/" ^ dirname ^ "z3_result.tmp" in
  let _ = Sys.command("./run_z3 " ^ z3_inputpath ^ " > " ^ z3_outputpath) in
  let result_file = open_in z3_outputpath in
  let result_string = input_line result_file in
  let _ = print_endline(result_string) in
  (result_string = "unsat")
  

