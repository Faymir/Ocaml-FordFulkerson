(* Read a graph from a file,
 * Write a graph to a file. *)

open Graph

type path = string

(* Values are read as strings. *)
val from_file: path -> string graph
(* val add_node_arc: string graph -> id id 'a *)
val from_txt_file: path -> string graph

(* Similarly, we write only a string graph.
 * Use Graph.map if necessary to prepare the input graph. *)
val write_file: path -> string graph -> unit

val export: path -> string graph -> string -> string -> unit