open Graph
open Algo
let () =

  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
    end ;

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)
  
  (* These command-line arguments are not used for the moment. *)
  and source = Sys.argv.(2)
  and sink = Sys.argv.(3)
  in

  (* Open file *)
  let graph = Gfile.from_file infile in

  (* Rewrite the graph that has been read. *)
  let () = Gfile.export outfile graph source sink in
  let alpha = ford_fulkerson graph source sink in 
  Printf.printf "%d\n" alpha;
  ()


