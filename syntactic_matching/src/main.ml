(*
 * boolean eqn to ocamlgraph
 *)

open Graph_circuit
open Unify
open Graph_opt

exception Error of string
let filename = Sys.argv.(1)
let case_filename = Sys.argv.(2)

(*let _ = Sys.command("mkdir ./z3_tmpdir/" ^ filename)*)

let start_time = Unix.time()

let eqn2circuit : string -> Circuit.circuit = fun filename ->
  let input_eqn = open_in (filename) in
  let lexbuf = Lexing.from_channel input_eqn in
  let circuit = EqnParser.main EqnLexer.token lexbuf in
  circuit

let tgt_cir = eqn2circuit filename
let tgt_graph = cir2graph tgt_cir
let _ = print_endline("cir to graph finished")


let (ilist, olist, elist) = tgt_cir
let vlist_to_print = (List.map fst elist)
(*
let _ =
  List.map
    (fun v -> print_string(v ^ " is valid : "); print_endline (string_of_bool (is_valid_node tgt_graph (Node.VAR v, 1))))
    vlist_to_print
*)

let _ = print_endline("old graph depth")
let _ = graph_depth_print tgt_graph olist

let tgt_vlist = List.map fst elist
let new_graph = graph_opt_by_case_file tgt_graph filename case_filename

let _ = print_endline("graph opt finished")
let _ = Eqn_printer.print_graph new_graph

let _ = print_endline("new graph depth")
let _ = graph_depth_print new_graph olist

let old_depth = graph_max_depth tgt_graph olist
let _ = print_endline("old mult depth : " ^ (string_of_int old_depth))
let new_depth = graph_max_depth new_graph olist
let _ = print_endline("new mult depth : " ^ (string_of_int new_depth))


let consumed_time = (int_of_float(Unix.time() -. start_time))
let consumed_hour = consumed_time / 3600
let consumed_min = (consumed_time mod 3600) / 60
let consumed_sec = consumed_time mod 60
let time_string = ""
let time_string = if(0 < consumed_hour) then time_string ^ (string_of_int consumed_hour) ^ "h " else time_string
let time_string = 
  if(0 < consumed_min && consumed_min < 10) then 
    time_string ^ " " ^ (string_of_int consumed_min) ^ "m " 
  else if (10 <= consumed_min) then  
    time_string ^ (string_of_int consumed_min) ^ "m " 
  else if(0 < consumed_hour) then
    time_string ^ " 0m "
  else 
    time_string

let time_string =
  if(consumed_sec < 10) then 
    time_string ^ " " ^ (string_of_int consumed_sec) ^ "s" 
  else 
    time_string ^ (string_of_int consumed_sec) ^ "s" 


let filename = let regexp = Str.regexp_string "paper_bench/" in Str.global_replace regexp "" filename 
let filename_length = String.length filename
let time_length = String.length time_string
let old_depth_string = if (old_depth < 10) then " " ^ (string_of_int old_depth) else (string_of_int old_depth)
let new_depth_string = if (new_depth < 10) then " " ^ (string_of_int new_depth) else (string_of_int new_depth)

let rec empty_n_string n = if(n = 0) then "" else " " ^ empty_n_string (n-1) 
let _ = prerr_endline(filename ^ (empty_n_string (19 - filename_length)) ^ (old_depth_string) ^ "             " ^ (new_depth_string) ^ empty_n_string (17 - time_length) ^ time_string)


(* graph print test *) 

(*let _ =
  List.map
    (fun v -> print_string(v ^ " is valid : "); print_endline (string_of_bool (is_valid_node new_graph (Node.VAR v, 1))))
    vlist_to_print
*)

