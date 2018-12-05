open Graph
open Printf
type path = id list

(*Utilitaire:  Un find co=ustomisé qui retourne la valeur à recherchée si 
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
  let print_list l str = printf "%s" str; List.iter (fun x -> Printf.printf  " %s <= " x) l; printf "%s" str
    
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
let print_path path =  List.iter (fun x -> Printf.printf  " %s <- " x) path;   Printf.printf "\n"

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

let rec ford_fulkerson_job acu graph path source sink = 
  match path with
  | [] -> acu
  | p -> let flow = min_flow graph p in
        let graph = update_path graph p flow in
          ford_fulkerson_job (flow + acu) (graph) (path_exist graph source sink) source sink

let rec circulation_demand_job acu graph path source sink = 
  match path with
  | [] -> acu
  | p -> let flow = min_flow graph p in
    let graph = update_path graph p flow in
    circulation_demand_job ((flow,p) :: acu) (graph) (path_exist graph source sink) source sink
  
let circulation_demand graph source sink = 
  circulation_demand_job ([]) (map graph int_of_string) (path_exist graph source sink) source sink

let rec print_paths paths = 
  List.iter (fun (flow,path) -> Printf.printf  "flow = %d" flow; print_path path) paths;   Printf.printf "\n"

let ford_fulkerson graph source sink =
  ford_fulkerson_job (0) (map graph int_of_string) (path_exist graph source sink) source sink


(* let gr = [("0",[("1", 16);("2", 13)]) ; ("1",[("2",10);("3", 12)]) ; ("2",[("1", 4);("4", 14)]) ; ("3",[("2", 9); ("5", 20)]) ; ("4",[("3", 7); ("5", 4)]) ; ("5",[])] ;; *)