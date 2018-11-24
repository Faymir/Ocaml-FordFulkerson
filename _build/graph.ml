type id = string

type 'a out_arcs = (id * 'a) list

(* A graph is just a list of pairs: a node & its outgoing arcs. *)
type 'a graph = (id * 'a out_arcs) list

exception Graph_error of string

let empty_graph = []

let node_exists gr id = List.mem_assoc id gr

let out_arcs gr id =
  try List.assoc id gr
  with Not_found -> raise (Graph_error ("Node " ^ id ^ " does not exist in this graph."))

let find_arc gr id1 id2 =
  let out = out_arcs gr id1 in
  try Some (List.assoc id2 out)
  with Not_found -> None

let add_node gr id =
  if node_exists gr id then raise (Graph_error ("Node " ^ id ^ " already exists in the graph."))
  else (id, []) :: gr

let add_arc gr id1 id2 lbl =

  (* Existing out-arcs *)
  let outa = out_arcs gr id1 in

  (* Update out-arcs.
   * remove_assoc does not fail if id2 ilet v_fold gr f acu = List.fold_left (fun acu (id, out) -> f acu id out) acu grs not bound.  *)
  let outb = (id2, lbl) :: List.remove_assoc id2 outa in
  
  (* Replace out-arcs in the graph. *)
  let gr2 = List.remove_assoc id1 gr in
  (id1, outb) :: gr2

let v_iter gr f = List.iter (fun (id, out) -> f id out) gr

let v_fold gr f acu = List.fold_left (fun acu (id, out) -> f acu id out) acu gr

let rec map gr f = match gr with
  | [] -> []
  | (id, arcs) :: rest -> (id, ( List.map (fun (idd,x) -> (idd, f (x) )) arcs )) :: map rest f

let update_arc gr id1 id2 minval =
  let res = v_fold 
    gr 
    (fun acu id out -> 
      if id = id1 then 
        let updated_flow_out = List.map(fun (x,lbl) -> if x = id2 then (x, lbl - minval) else (x,lbl)) (out) in
          let filtered_out = List.filter(fun (x,lbl) -> lbl >0) (updated_flow_out) in
          (id,filtered_out) :: acu 
      else 
        (id,out) :: acu
    ) 
    [] 
  in
  List.rev res