open Graph
open Printf
type path = id list

(*Utilitaire:  Un find customisé qui retourne la valeur à recherchée si 
celle ci est trouvée ou une chaine vide dans le cas contraire*)
let myfind id mylist =  try (List.find (fun x -> x = id) mylist) with Not_found -> ""

(*Utilitaire: Fonction me permettant de Filtrer une liste de successeurs en retirant
 ceux déjà explorés ou déjà pris en compte pour l'exploration*)
  let myfilter id notyet explored = 
    let a = myfind id notyet and b = myfind id explored in
    if a <> "" || b <> "" then
      false (* ne pas ajouter cette valeur dans la nouvelle liste *)
    else
      true  (* Oui cette valeur ne se trouve dans aucune liste donc ajoute la dans la nouvelle liste filtrée*)

  (*DEBOGAGE: Juste une fonciton pour afficher les liste*)
  let print_list l str = 

    printf "%s" str; List.iter (fun x -> Printf.printf  " %s <= " x) l; printf "%s" str
    
  (*Utilitaire:  *)
  let rec boucle graph notyet explored goal result = 
    match notyet with
    | [] -> []  (*Condition d'arrêt de ma fonction:   Il n'y a pas de chemin entre id1 et id2*)
    | ":" :: rest ->  if result <> [] then   (*On retire du résultat, le chemin qui ne menais pas
                                               à goal en remontant jusqu'au dernier noeud parent ayant deux noeud fils dont celui ci *)
                        boucle graph rest explored goal (List.tl result)
                      else
                        boucle graph rest explored goal result
    | first :: rest ->    (* Algorithme principal de recherche en profondeur: on ajoute toujours
                             en début de la liste des noeuds à explorer, les nouveaux noeuds fils
                             pour etre sûr de toujours finir une branche avant de passer à la suivante*)
                      let notyet = rest in
                        if first = goal then goal :: result
                        else 
                          let successors = out_arcs graph first in
                            let successors = List.filter (fun (x,_) -> myfilter x notyet (first :: explored))  successors in
                            (*Ci dessus On filtre la liste des successeurs du noeuds actuel en
                             retirant les noeuds déjà exploré ou ceux déjà présent dans la liste
                              des noeuds à explorer*)
                              let notyet = List.append (List.map (fun (x,_) -> x) successors) (":" :: notyet) in
                                (*ci-dessus on marque chaque subdivision en noeud avec un caratere : pour pouvoir
                                 y revenir et supprimer un mauvais chemin: utilisé au niveau de la ligne 3 de cette fonction*)
                                boucle graph notyet (first :: explored) goal (first :: result)

(*Utilitaire: trouver et retourner un chemin*)   
let path_exist graph id1 id2 = boucle graph [id1] [] id2 []

(*Utilitaire: afficher un chemin depuis l'extérieur du module*)
let print_path path =     
 let path = List.rev path in
  List.iter (fun x -> Printf.printf  "\t-> %s" x) path;   Printf.printf "\n"

let rec print_paths paths = 
  let acu = 0 and paths = List.rev paths in
  Printf.printf "Total flow: %d\n" 
    (List.fold_left (fun acu (flow,path) ->  
      if flow > 0 then (Printf.printf  "Flow of %d can be applied in:" flow;  print_path path; acu + flow)
      else acu + flow)
    acu paths)

let rec update_path_inverse graph graph2 path minval=
  match path with
  | [] -> (graph,graph2)
  | id :: [] -> (graph,graph2)
  | id2 :: id1 :: rest -> let graph = update_arc graph id1 id2 minval and graph2 = update_arc graph2 id1 id2 minval in
                            let graph2 = add_arc graph2 id2 id1 minval in
                                update_path_inverse (graph)(graph2)(id1 :: rest) minval

let rec update_path graph path minval=
  match path with
  | [] -> graph
  | id :: [] -> graph
  | id2 :: id1 :: rest -> update_path (update_arc graph id1 id2 minval)(id1 :: rest) minval

let rec calc_flow graph path acu first =
  match path with
  | [] -> acu
  | id :: [] -> acu

  | id2 :: id1 :: rest -> let value = find_arc graph id1 id2 in
                            match value with
                            | Some v -> if first = 0 then
                                          calc_flow (graph) (id1::rest) (v) 1
                                        else if v < acu then
                                          calc_flow (graph) (id1::rest) (v) 1
                                        else
                                          calc_flow (graph) (id1::rest) (acu) 1
                            | None -> raise (Graph_error ("This Error From calc_flow lean to find_arc and path_exist Should Not Happen."))

let rec min_flow graph path =
  calc_flow graph path 0 0

let rec algorithm acu graph graph2 path source sink =
  let rec check_reverse acu graph path source sink = 
    match path with
    | [] -> acu
    | p -> let flow = min_flow graph p in
      let graph = update_path graph p flow in
      check_reverse ((flow,p) :: acu) (graph) (path_exist graph source sink) source sink
  in
  match path with
  | [] -> (*let () = Gfile.export ("graph2.dot") (map graph2 string_of_int) "0" "8"  in *)
          check_reverse (acu) (graph2) (path_exist graph2 source sink) source sink

  | p -> let flow = min_flow graph p in
    let (graph,graph2) = update_path_inverse graph graph2 p flow in
    algorithm ((flow,p) :: acu) (graph) (graph2) (path_exist graph source sink) source sink


let ford_fulkerson graph source sink =
let graph = map graph int_of_string in
  let paths = algorithm ([]) (graph) (graph) (path_exist graph source sink) source sink and acu = 0 in
    (List.fold_left (fun acu (flow,_) ->  acu + flow) acu paths)
  
let circulation_demand graph source sink = 
  let graph = map graph int_of_string in
    let paths = algorithm ([]) (graph) (graph) (path_exist graph source sink) source sink in
      let () = print_paths paths and acu = 0 in
        (List.fold_left (fun acu (flow,_) ->  acu + flow) acu paths)



(* let gr = [("0",[("1", 16);("2", 13)]) ; ("1",[("2",10);("3", 12)]) ; ("2",[("1", 4);("4", 14)]) ; ("3",[("2", 9); ("5", 20)]) ; ("4",[("3", 7); ("5", 4)]) ; ("5",[])] ;; *)