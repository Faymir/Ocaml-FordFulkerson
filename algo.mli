open Graph

type path = id list
val path_exist: 'a graph -> id -> id -> path
val print_path: path  -> unit