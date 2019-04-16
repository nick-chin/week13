open W13_q1

let g = gen_random_graph 50

let test_bfs g =
  let all_nodes = get_nodes g in
  let (bfs_roots, _, _, _) = GraphBFS.bfs g in

  (* Any node BFS-reachable from a root r is reachable from r *)
  let fact1 =
    List.for_all bfs_roots ~f:(fun u ->
        List.for_all all_nodes ~f:(fun v ->
            if GraphBFS.is_reachable_via_bfs g u v
            then is_reachable g u v
            else true))
  in

  (* Any node is reachable from some root r *)
  let fact2 =
    List.for_all all_nodes ~f:(fun u ->
        List.exists dfs_roots
          ~f:(fun r -> GraphBFS.is_reachable_via_bfs g r u)) in

  fact1 && fact2
  
  



open W13_q2

let%test "Random Knight Path" =
  random_knight_path ();
  true


open W13_q5
open Week_13_Spanning

let%test "Testing Random Spanning Tree" =
  let g = example_graph_undirected in
  let n = ref 100 in
  let counter = ref true in
  while !n <> 0 do
    counter := !counter && test_rst_size g && test_rst_connectness g && test_rst_connectness g && test_compare_weight_rst_mst g;
    n := !n - 1;
  done;
  !counter
