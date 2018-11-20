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
    printf "%s" str;
    List.iter (fun x -> Printf.printf  " %s <= " x) l;
    printf "%s" str

  let rec boucle graph notyet explored goal result = 
    print_list notyet "\tNotYet\t";
    print_list explored "\tExplored\t";
    print_list result "\tresult\t";
    print_list memoire "\nMémoire\n";
    match notyet with
    | [] -> ["Not_found"]
    | ":" :: rest ->  if result <> [] then
                        boucle graph rest explored goal (List.tl result)
                      else
                        boucle graph rest explored goal result
    | first :: rest -> 
      let notyet = rest in
      printf "\nFirst = %s\t goal = %s\n" first goal;
          if first = goal then goal :: result
          else 
            let successors = out_arcs graph first in
              let successors = List.filter (fun (x,_) -> myfilter x notyet (first :: explored))  successors in
                let notyet = List.append (List.map (fun (x,_) -> x) successors) (":" :: notyet) in
                  boucle graph notyet (first :: explored) goal (first :: result)
              
let path_exist graph id1 id2 = 
    let notyet = [id1] and explored = [] in
    boucle graph notyet explored id2 []

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
 (* printf "\nDebut successors of %s\n" first;
                  List.iter (fun (x,_) -> printf " %s -> " x) successors;
                  printf "\nfin successors\n"; *)
                  (* printf "\n- successors --\n" ;
                  List.iter (fun (x,_) -> printf " %s -> " x) successors;
                  printf "\nfin successors\n";
                  print_list explored; *)