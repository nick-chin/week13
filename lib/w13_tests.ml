open W13_q2

let%test "Random Knight Path" =
  random_knight_path ();
  true


open W13_q5
open Week_13_Spanning
open Week_12_Graphs
  
let%test "Testing Spanning Tree Size" = 
  let t = random_spanning_tree_finder example_graph_undirected in 
  List.length t = v_size example_graph_undirected - 1

let%test "Testing Spanning Tree Cycle" = 
  let g = example_graph_undirected in
  let tree = random_spanning_tree_finder g in
  let forest = mk_UF (v_size g) in
  let rec check_cycle e =
  match e with
  | [] -> true
  | [(u,v)] -> (find forest u) <> (find forest v)
  | (u,v) :: t -> 
    let su = find forest u in
    let sv = find forest v in
    if su <> sv then (union forest u v; check_cycle t) else false in
check_cycle tree

let%test "Testing Spanning Tree Connectness" = 
  let g = example_graph_undirected in
  let t = random_spanning_tree_finder g in
  match t with
  | [] -> true
  | _ ->
  let t_graph = mk_graph (v_size g) in
  let nodes = Set.elements g.nodes in
  List.iter (fun n -> add_node t_graph n) nodes;
  List.iter (fun (a,b) -> add_edge t_graph a b) t;
  List.iter (fun (a,b) -> add_edge t_graph b a) t;
  List.for_all (fun n -> is_reachable t_graph (List.hd nodes) n) nodes

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
