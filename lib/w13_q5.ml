open Week_13_Reachability
open Week_13_Spanning
open Week_13_Paths
open Week_12_BST
open BinarySearchTree
open Week_11_UnionFind
open Week_12_Graphs
open LinkedGraphs
open UnionFind

let random_spanning_tree_finder g =
  let forest = mk_UF (v_size g) in
  let tree = ref [] in
  let edges = Set.elements g.edges in
  let shuffle l =
    let nd = List.map (fun c -> (Random.bits (), c)) l in
    let sond = List.sort compare nd in
    List.map snd sond in
  List.iter (fun (u, v) ->
      let su = find forest u in
      let sv = find forest v in
      if su <> sv
      then begin
        tree := (u, v) :: !tree;
        union forest u v
      end) (shuffle edges);
  !tree

let test_rst_size g =
  let t = random_spanning_tree_finder g in
  List.length t = v_size g - 1

let test_rst_cycle g =
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

let test_rst_connectness g = 
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

let test_compare_weight_rst_mst g =
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
