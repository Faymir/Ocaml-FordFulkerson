open Graph
open Printf
type path = id list
let isforbidden id forbidden =  try (List.find (fun x -> x = id) forbidden) with Not_found -> ""

(*let rec check_out acu out_arcs graph forbidden s p = 
  match out_arcs with
  | (id, _) :: rest -> 
  if (isforbidden id forbidden) == "" then
    if rest == [] then
      "__"
    else
      check_out acu rest graph forbidden s p  
  else if id == p then acu 
  else let bbb = id :: acu in check_out (bbb) (out_arcs graph id) (graph) (id::forbidden) (s) (p)  (* sinnon ajouter a acu et verifier la suite *)
let path_exist graph forbidden s p =
*)
  let myfind id mylist =  try (List.find (fun x -> x = id) mylist) with Not_found -> ""

  let myfilter id notyet explored = 
    let a = myfind id notyet and b = myfind id explored in
    if a <> "" || b <> "" then
      false (* ne pas ajouter cette valeur dans la nouvelle liste *)
    else
      true  (* Oui cette valeur ne se trouve dans aucune liste donc ajoute la dans la nouvelle lisye filtrée*)

  let print_list l str=
    printf "\nBeginning %s\n" str;
    List.iter (fun x -> Printf.printf  " %s <= " x) l;
    printf "\nFIN %s\n" str
  
  let rec clean_result result nbr = 
    match nbr with
    | 0 -> result
    | number -> if result = [] then [] else clean_result (List.tl result) (nbr - 1)

  let rec boucle graph notyet explored goal result nbr= 
    print_list notyet "NotYet";
    print_list explored "Explored";
    print_list result "result";
    match notyet with
    | [] -> ["Not_found"]
    | ":" :: rest -> 
        if nbr = 0 then boucle graph rest explored goal result 0
        else boucle graph rest explored goal (("(" ^ (string_of_int nbr) ^ ")") :: result) 0
        (* else boucle graph rest explored goal (clean_result result nbr) 0 *)
    | first :: rest -> 
      let notyet = rest in
      printf "\nFirst = %s\t goal = %s\n" first goal;
          if first = goal then goal :: result
          else 
            let successors = out_arcs graph first in

              let explored = first :: explored in


                let successors = List.filter (fun (x,_) -> myfilter x notyet explored)  successors in
                  (* let nbr = nbr + 1 in *)
                  (* printf "\nDebut successors of %s\n" first;
                  List.iter (fun (x,_) -> printf " %s -> " x) successors;
                  printf "\nfin successors\n"; *)
                  (* printf "\n- successors --\n" ;
                  List.iter (fun (x,_) -> printf " %s -> " x) successors;
                  printf "\nfin successors\n";
                  print_list explored; *)
                  let notyet = List.append (List.map (fun (x,_) -> x) successors) (":" :: notyet) in

                    (* print_list notyet "NotYet"; *)
                  if successors <> [] then 
                    boucle graph notyet explored goal ( ("(" ^ (string_of_int nbr) ^ ")") :: first :: result) 0
                  else
                    boucle graph notyet explored goal (first :: result) (nbr + 1)
              
let path_exist graph id1 id2 = 
    let notyet = [id1] and explored = [] in
    boucle graph notyet explored id2 [] 0

let print_path path =
  List.iter (fun x -> Printf.printf  " %s <- " x) path; 
  Printf.printf "\n"

  (*  

      - on quitte si on trouve la solution ou si on n'a plus de noeuds à exporer
      - Deux listes: noeux explorés, noeuds non explorés
      - on selectionne le premier noeud
      - on le supprime de la liste des noeud à explorer
      - on genere une liste de de successeurs du noeuds actuel 
      - on verifie si le noeud actuel est solution: si oui exit loop
      - sinon 
        - on l'ajoute à la liste des noeud exploré
        - on filtre la liste de successeurs pour eliminer tout noeud contenus **
          dans ceux explorés et ceux non explorés
        - on ajoute au debut de la liste des noeuds à exploré la liste filtrée de successeur
       - on boucle jusqu'à solution ou liste d'exploration vide



      type 'a out_arcs = (id * 'a) list
      type 'a graph = (id * 'a out_arcs) list
                      (id * ((id * 'a) list) )




                      let rec check_out acu m_out_arcs graph forbidden s p = 
  match m_out_arcs with
  | (id, _) :: rest -> 
  if (isforbidden id forbidden) then
    if rest == [] then
      raise Not_found
    else
      check_out acu rest graph forbidden s p  
  else if id == p then acu 
  else let bbb = id :: acu in check_out (bbb) (out_arcs graph id) (graph) (id::forbidden) (s) (p)  (* sinnon ajouter a acu et verifier la suite *);;
  *)
  (* match graph with
  | [] -> None
  | (s,o_arcs) :: rest -> v_fold 
  | (id1,_) :: rest -> path_exist rest (id1::forbidden) s p *)
