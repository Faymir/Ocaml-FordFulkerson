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
  let print_list l str=
    printf "%s" str;
    List.iter (fun x -> Printf.printf  " %s <= " x) l;
    printf "%s" str
    
  (*Utilitaire:  *)
  let rec boucle graph notyet explored goal result = 
    match notyet with
    | [] -> []  (*Condition d'arrêt de ma fonction:   Il n'y a pas de chemin entre id1 et id2*)
    | ":" :: rest ->  if result <> [] then   (*En retir du résultat le chemin qui ne menais pas
                                               à goal en remontant jusqu'au noeud dernier parent ayant deux noeud fils dont celui ci *)
                        boucle graph rest explored goal (List.tl result)
                      else
                        boucle graph rest explored goal result
    | first :: rest ->    (* Algorithme principale de recherche en profondeur on ajoute toujours
                             en début de la liste des noeuds à explorer les nouveaux noeuds fils
                             pour etre sur de toujours finir une branche avant de passer à la suivante*)
                      let notyet = rest in
                        if first = goal then goal :: result
                        else 
                          let successors = out_arcs graph first in
                            let successors = List.filter (fun (x,_) -> myfilter x notyet (first :: explored))  successors in
                            (*Ci dessus On filtre la liste des successeurs du noeuds actuel en
                             retirant les noeuds déjà exploré ou ceux déjà présent dans la liste
                              des noeuds à explorer*)
                              let notyet = List.append (List.map (fun (x,_) -> x) successors) (":" :: notyet) in
                                boucle graph notyet (first :: explored) goal (first :: result)

(*Utilitaire: trouver et retourner un chemin*)   
let path_exist graph id1 id2 = 
    let notyet = [id1] and explored = [] in
    boucle graph notyet explored id2 []

(*Utilitaire: afficher un chemin depuis l'extérieur du module*)
let print_path path =
  List.iter (fun x -> Printf.printf  " %s <- " x) path; 
  Printf.printf "\n"

(* Remove only if arc is on path and if its flow of is less than, or equal to 0 *)
let remove_link graph id1 id2 minval=
  let res = v_fold 
    graph 
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

let rec remove_path graph path minval=
  match path with
  | [] -> graph
  | id :: [] -> graph
  | id2 :: id1 :: rest -> remove_path ( 
                                         remove_link graph id1 id2 minval 
                                      ) 
                                      (id1 :: rest) 
                                      minval

let rec calc_flow graph path acu first =
  match path with
  | [] -> acu
  | id :: [] -> acu
  | id2 :: id1 :: rest -> let value = find_arc graph id1 id2 in
                            if value = None then 
                              raise (Graph_error ("This Error From calc_flow lean to find_arc and path_exist Should Not Happen."))
                            else
                              let Some v = value in 
                                if first = 0 then
                                  calc_flow (graph) (id1::rest) (v) 1
                                else if v < acu then
                                  calc_flow (graph) (id1::rest) (v) 1
                                else
                                  calc_flow (graph) (id1::rest) (acu) 1

let rec min_flow graph path =
  calc_flow graph path 0 0

let ford_ferguson_job acu graph path source sink = 
  match path with
  [] -> acu
  p -> let flow = calc_flow graph p 0 0 in
        let graph = remove_path graph p flow in
          ford_ferguson (flow + acu) (graph) (path_exist graph source sink) source sink






(* let gr = [("0",[("1", 16);("2", 13)]) ; ("1",[("2",10);("3", 12)]) ; ("2",[("1", 4);("4", 14)]) ; ("3",[("2", 9); ("5", 20)]) ; ("4",[("3", 7); ("5", 4)]) ; ("5",[])] ;; *)