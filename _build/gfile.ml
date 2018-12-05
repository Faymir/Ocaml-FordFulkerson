open Graph
open Printf
    
type path = string

(* Format of text files: lines of the form 
 *
 *  v id               (node with the given identifier)
 *  e label id1 id2    (arc with the given (string) label. Goes from node id1 to node id2.)
 *
 *)

let write_file path graph =

  (* Open a write-file. *)
  let ff = open_out path in

  (* Write in this file. *)
  fprintf ff "=== Graph file ===\n\n" ;

  (* Write all nodes *)
  v_iter graph (fun id _ -> fprintf ff "v %s\n" id) ;
  fprintf ff "\n" ;

  (* Write all arcs *)
  v_iter graph (fun id out -> List.iter (fun (id2, lbl) -> fprintf ff "e \"%s\" %s %s\n" lbl id id2) out) ;
  
  fprintf ff "\n=== End of graph ===\n" ;
  
  close_out ff ;
  ()

(* Reads a line with a node. *)
let read_node graph line =
  try Scanf.sscanf line "v %s" (fun id -> add_node graph id)
  with e ->
    Printf.printf "Cannot read node in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "from_file"

(* Reads a line with an arc. *)
let read_arc graph line =
  try Scanf.sscanf line "e \"%s@\" %s %s" (fun label id1 id2 -> add_arc graph id1 id2 label)
  with e ->
    Printf.printf "Cannot read arc in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "from_file"

let read_txt_arc graph line =
  try 
    let (id1, id2, label) = Scanf.sscanf line "e %s %s %s" (fun id1 id2 label -> (id1, id2, label)) in
      let graph = 
                if (node_exists graph id1) = false then
                  add_node graph id1
                else if (node_exists graph id2) = false then
                  add_node graph id2
                else
                  graph
      in
        add_arc graph id1 id2 label
  with e ->
    Printf.printf "Cannot read arc in line - %s:\n%s\n" (Printexc.to_string e) line ;
    failwith "from_file"

let from_txt_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop graph =
    try
      let line = input_line infile in
      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph

        (* The first character of a line determines its content : v or e.
         * Lines not starting with v or e are ignored. *)
        else match line.[0] with
          | 'e' -> read_txt_arc graph line
          | _ -> graph
      in                 
      loop graph2        
    with End_of_file -> graph
  in

  let final_graph = loop empty_graph in

  close_in infile ;
  final_graph

let from_file path =

  let infile = open_in path in

  (* Read all lines until end of file. *)
  let rec loop graph =
    try
      let line = input_line infile in
      let graph2 =
        (* Ignore empty lines *)
        if line = "" then graph

        (* The first character of a line determines its content : v or e.
         * Lines not starting with v or e are ignored. *)
        else match line.[0] with
          | 'v' -> read_node graph line
          | 'e' -> read_arc graph line
          | _ -> graph
      in                 
      loop graph2        
    with End_of_file -> graph
  in

  let final_graph = loop empty_graph in
  
  close_in infile ;
  final_graph
  
  let  export path graph source sink= 
    (*let path :: _  = String.split_on_char('.') in*)
    let ff = open_out path in

    (* Write in this file. *)
    fprintf ff "digraph result {\n\trankdir=LR;\n\tsource %s\n\tsink %s\n\tsize=\"8,5\"\n\tnode [shape = circle];\n" source sink;
    
    v_iter graph (fun id out -> List.iter (fun (id2, lbl) -> fprintf ff "\t\t%s -> %s [ label = \"%s\" ];\n" id id2 lbl) out) ;
    
    fprintf ff "}";
    close_out ff ;
    ()