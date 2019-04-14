open Week_01
open Week_06
open Week_13_Paths
open Core_kernel
open Week_12_Graphs
open Week_13_Reachability
open Week_12_BST
open BinarySearchTree
open LinkedGraphs

module GraphBFS = struct
  open NodeTable
  open Distance
  open DLLBasedQueue

  type color = White | Gray | Black

  let bfs g s =
    let color_map = mk_new_table (v_size g) in
    let parent_map = mk_new_table (v_size g) in
    let tree_map = mk_new_table (v_size g) in
    let distance_map = mk_new_table (v_size g) in
    let has_cycles = ref false in
    let roots = ref [] in
    let all_nodes = get_nodes g in

    (* Make all nodes white *)
    List.iter all_nodes (fun n -> insert color_map n White) ;
    (* Make all distances Infinity *)
    List.iter all_nodes (fun n -> insert distance_map n Infinity) ;
    (*Make the parent of each node None*)
    List.iter all_nodes (fun n -> insert parent_map n None) ;


    (*Make color of s gray*)
    insert color_map s Gray;
    (*Make distance of s Finite 0)*)
    insert distance_map s (Finite 0);
    (*Make parent of S None*)
    insert parent_map s (None);


    (* Make a queue *)
    let q = mk_queue (v_size g);
    (*Enuqueue (q,s)*)
    enqueue q s;


    while not (is_empty q) do (
     let u = get_exn (dequeue q) in
     let lst = get_succ g u in

(*Perform action on all adjacent vertices*)

(*Whenever a white vertex is found while scanning for u, the vertex v
and the edge (u, v) are added to the tree.*)
    lst |> List.iter (fun v ->
        let v_color = get_exn @@ get color_map v in
        if v_color = White
        then begin
          insert color_map v Gray;
          insert distance_map v ((get_exn @@ get distance_map u) + Finite 1);
          insert parent_map v (Some u);
          let siblings = get_exn @@ get tree_map u in
          insert tree_map u (v :: siblings);
          enqueue q v;
          end;
    else if v_color = Gray
        then has_cycles := true;
    else if v_color = Black
        then has_cycles := true)
      insert color_map u Black; 
        done)


(* We iterate through all the nodes with white color*)

List.iter (fun n ->
      if get_exn @@ get color_map n = White
      then begin
        (* Record roots *)
        roots := n :: !roots;
        bfs g n
      end)
    all_nodes;
    (!roots, parent_map, distance_map, tree_map);
end
