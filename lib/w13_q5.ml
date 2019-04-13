open Core_kernel
open Week_12_Graphs
open Week_13_Reachability
open Week_12_BST
open BinarySearchTree
open Week_11_UnionFind
open LinkedGraphs

let random_spanning_tree_finder g =
  let open UnionFind in
  let forest = mk_UF (v_size g) in
  let tree = ref [] in
  let edges = Set.elements g.edges in
  let shuffle l =
    let nd = List.map (fun c -> (Random.bits (), c)) l in
    let sond = List.sort compare nd in
    List.map snd sond in
  List.iter (shuffle edges) ~f:(fun (u, v) ->
      let su = find forest u in
      let sv = find forest v in
      if su <> sv
      then begin
        tree := (u, v) :: !tree;
        union forest u v
      end);
  !tree