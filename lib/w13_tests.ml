open W13_q2

let%test "Random Knight Path" =
  random_knight_path ();
  true

open W13_q5


open Week_13_Spanning
  
let%test "Testing Spanning Tree Size" = 
  let t = random_spanning_tree_finder example_graph_undirected in 
  List.length t = v_size example_graph_undirected - 1

let%test "Testing Spanning Tree Cycle" =  
  let g = LinkedGraphs.parse_linked_int_graph medium_graph_shape in
  let (_, _, _, c) = GraphDFS.dfs g in
  not c

let%test "Testing Spanning Tree Connectness" = 
  let g = example_graph_undirected in
  let t = random_spanning_tree_finder g in
  match t with
  | [] -> true
  | (u, v) :: _ ->
  let nodes = Set.elements g.nodes in
  List.for_all (fun n -> is_reachable g u n) nodes

let%test "Testing Spanning Tree Weight" = 
  let g = example_graph_undirected in
  let rst = random_spanning_tree_finder g in
  let mst = mst_kruskal g in
  let rst_weight = List.map (fun (a,b) -> get_linked_edge_label g a b) rst in
  let mst_weight = List.map (fun (a,b) -> get_linked_edge_label g a b) mst in
  let rec sum wl =
    match wl with
    | [] -> 0
    | h :: t -> h + (sum t)
  in
  (sum rst_weight) >= (sum mst_weight)
