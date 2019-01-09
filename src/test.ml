
open OUnit2;;
let folder = "../in/test/";;
let files = ["graph1.txt";    "graph2.txt";     "graph3.txt";     "graph4.txt"];;
(* check flow [(from node, to node, right result flow):: _]  *)
let paths = [("0", "3", 6);   ("0", "7", 5);    ("0", "7", 31);   ("0", "5", 14)];; 
(*
graph1 (0 - 3) -> flow = 6
graph2 (0 - 7) -> flow = 5
graph3 (0 - 7)-> flow = 31
graph4 (0 - 5)-> flow = 14
 *)

 let rec get_graphs acu files =
 match files with
  | [] -> List.rev acu
  | file :: rest -> get_graphs ((Gfile.from_txt_file (folder ^ file)) :: acu) (rest)

 let graphs = get_graphs [] files;;

 let get_assert choice flow graph source sink = 
  if choice = 1 then 
    assert_equal ~printer:string_of_int flow (Algo.ford_fulkerson graph source sink)
  else if choice = 2 then
    assert_equal ~printer:string_of_int flow (Algo.circulation_demand graph source sink);;

 let rec make_tests choice graphs files paths =
 match (graphs, paths, files) with
 | ([], _, _) -> ()
 | (_, [], _) -> ()
 | (_, _, []) -> ()
 | (graph::rest_graph, (source, sink, flow) :: rest_path, file::rest_files) -> 
            let test test_ctxt = get_assert choice flow graph source sink in
                let () = run_test_tt_main (file >:: test) in
                make_tests choice rest_graph rest_files rest_path;;

let () = make_tests 1 graphs files paths;;