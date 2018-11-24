open Graph

type path = id list


  (*  
        PRINCIPE DE FONCTIONNEMENT DE MA RECHERCHE DE CHEMIN
      
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
*)

val path_exist: 'a graph -> id -> id -> path
val print_path: path  -> unit
val update_path: int graph -> path -> int -> int graph
val min_flow: int graph -> path -> int
val ford_fulkerson:  string graph -> id -> id -> int