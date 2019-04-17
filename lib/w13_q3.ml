(* Exercise 3 *)

open Week_01
open Week_02
open Week_03
open Week_06
open DLLBasedQueue
open Week_08_HashTable
open Week_10_ReadingFiles
open Week_12_BST
open BinarySearchTree
open Week_12_Graphs
open Week_13_Reachability
open LinkedGraphs
open Week_13_Paths
open NodeTable
open Set

(* function in module, corrected typo *)
let rec get_path_weight g path = match path with
  | (u, v) :: t -> 
    let w = get_linked_edge_label g u v in 
    w + get_path_weight g t
  | _ -> 0

(* relax edge (u, v) in increasing order *)
let relax_increasing dist_table prev_tree w u v =
  let open Distance in
  (* distance from s to v through u *)
  let vud = dist dist_table u + (Finite (w u v)) in
  let pred_u = get prev_tree u in
  (* update if and only if going from s to v through u is shorter and the distance from u to v is longer than the incoming edge to u in the current path *)
  if (dist dist_table v > vud) && ((pred_u <> None && Finite (w u v) > Finite (w (get_exn pred_u) u)) || (pred_u = None))
  then
    begin
      NodeTable.insert dist_table v vud;
      NodeTable.insert prev_tree v u
    end

(* relax edge (u, v) in decreasing order *)
let relax_decreasing dist_table prev_tree w u v =
  let open Distance in
  (* distance from s to v through u *)
  let vud = dist dist_table u + (Finite (w u v)) in
  let pred_u = get prev_tree u in
  (* update if and only if going from s to v through u is shorter and the distance from u to v is shorter than the incoming edge to u in the current path *)
  if (dist dist_table v > vud) && ((pred_u <> None && Finite (w u v) < Finite (w (get_exn pred_u) u)) || (pred_u = None))
  then
    begin
      NodeTable.insert dist_table v vud;
      NodeTable.insert prev_tree v u
    end

(* find increasing shortest path from node s to every other node in g *)
let increasing_shortest_path g s =
  let open Distance in
  let (w, d, p) = initialise_single_source g s in
  let all_edges = elements g.edges in
  for i = 0 to v_size g - 1 do
    List.iter (fun (u, v) -> relax_increasing d p w u v) all_edges
  done;
  let rec check_neg_cycles es =
    match es with
    | [] -> true
    | (u, v) :: t ->
      if dist d v > dist d u + (Finite (w u v))
      then false
      else check_neg_cycles t
  in
  ((p, d), check_neg_cycles all_edges)

(* find decreasing shortest path from node s to every other node in g *)
let decreasing_shortest_path g s =
  let open Distance in
  let (w, d, p) = initialise_single_source g s in
  let all_edges = elements g.edges in
  for i = 0 to v_size g - 1 do
    List.iter (fun (u, v) -> relax_decreasing d p w u v) all_edges
  done;
  let rec check_neg_cycles es =
    match es with
    | [] -> true
    | (u, v) :: t ->
      if dist d v > dist d u + (Finite (w u v))
      then false
      else check_neg_cycles t
  in
  ((p, d), check_neg_cycles all_edges)

(* get monotonic shortest path from s to t in g *)
let monotonic_shortest_path g s t =
  let ((increasing_p, increasing_d), increasing_bool) = increasing_shortest_path g s in
  let ((decreasing_p, decreasing_d), decreasing_bool)  = decreasing_shortest_path g s in
  let increasing_path = get_shortest_path increasing_p s t in
  let decreasing_path = get_shortest_path decreasing_p s t in
  match (increasing_path, decreasing_path) with
  | (None, None) -> None
  | (None, Some path) ->
     Some path
  | (Some path, None) ->
     Some path
  | (Some path1, Some path2) ->
     let weight1 = get_path_weight g path1 in
     let weight2 = get_path_weight g path2 in
     if weight1 < weight2
     then Some path1
     else if weight1 > weight2
     then Some path2
     else Some path1
     
(*
let g = example_graph_bf
monotonic_shortest_path g 0 1
monotonic_shortest_path g 0 2
monotonic_shortest_path g 0 3
monotonic_shortest_path g 0 4
*)

(* determine if final is reachable from init via an increasing path *)
let increasing_reachable g init final =
  let w = get_linked_edge_label g in
  let rec walk path visited n = 
    if n = final 
    then Some path
    else if List.mem n visited 
    then None
    else
      let successors =  get_succ g n in
      let visited' = n :: visited in
      let rec iter = function 
        | [] -> None 
        | h :: t ->
           if path = [] || (w n h) > w (fst (List.hd path)) (snd (List.hd path))
           then
             let path' = (n, h) :: path in
             match walk path' visited' h with
             | None -> iter t
             | Some p -> Some p
           else
             iter t
      in iter successors
  in
  match walk [] [] init with
  | Some p -> Some (List.rev p)
  | _ -> None

(*
increasing_reachable g 0 1
increasing_reachable g 0 2
increasing_reachable g 0 3
increasing_reachable g 0 4
*)

(* determine if final is reachable from init via a decreasing path *)
let decreasing_reachable g init final =
  let w = get_linked_edge_label g in
  let rec walk path visited n = 
    if n = final 
    then Some path
    else if List.mem n visited 
    then None
    else
      let successors =  get_succ g n in
      let visited' = n :: visited in
      let rec iter = function 
        | [] -> None 
        | h :: t ->
           if path = [] || (w n h) < w (fst (List.hd path)) (snd (List.hd path))
           then
             let path' = (n, h) :: path in
             match walk path' visited' h with
             | None -> iter t
             | Some p -> Some p
           else
             iter t
      in iter successors
  in
  match walk [] [] init with
  | Some p -> Some (List.rev p)
  | _ -> None

(*
decreasing_reachable g 0 1
decreasing_reachable g 0 2
decreasing_reachable g 0 3
decreasing_reachable g 0 4
*)

(* determine if final is reachable from init via a monotonic path *)
let monotonically_reachable g init final =
  let increasing = increasing_reachable g init final in
  let decreasing = decreasing_reachable g init final in
  match (increasing, decreasing) with
  | (None, None) -> None
  | (Some path, None) -> Some path
  | (None, Some path) -> Some path
  | (Some path1, _) -> Some path1
(*
monotonically_reachable g 0 1
monotonically_reachable g 0 2
monotonically_reachable g 0 3
monotonically_reachable g 0 4
*)

let is_monotonically_reachable g init final = 
  monotonically_reachable g init final <> None

(*****************************************)
(*                Tests                  *)
(*****************************************)

(* path is monotonic*)
let test_path_monotonic g s t =
  match monotonic_shortest_path g s t with
  | None -> true
  | Some path ->
     let w = get_linked_edge_label g in
     let weights = List.map (fun e -> w (fst e) (snd e)) path in
     let rec walk p acc =
       match p with
       | [] -> acc
       | x :: [] -> acc
       | x :: y :: t -> walk (y :: t) ((x - y) :: acc)
     in
     let mono = walk weights [] in
     List.for_all (fun x -> x > 0) mono ||
       List.for_all (fun x -> x < 0) mono

(* path is connected *)
let test_path_connected g s t =
  match monotonic_shortest_path g s t with
  | None -> true
  | Some path ->
     let rec walk p =
       match p with
       | (u, v) :: (x, y) :: t ->
          v = x && walk ((x, y) :: t)
       | _ -> true
     in
     walk path

(* all edges in the path are in the graph *)
let test_that_is_path_graph g s t =
  match monotonic_shortest_path g s t with
  | None -> true
  | Some path ->
    let all_edges = elements g.edges in
    List.for_all (fun e -> List.mem e all_edges) path

(* path exists for any monotonically reachable node *)
let test_monotonically_reachable_hence_has_path g s t =
  if is_monotonically_reachable g s t
  then monotonic_shortest_path g s t <> None
  else true

(* path is always shorter *)
let test_shortest_is_shorter g s t =
  match monotonically_reachable g s t with
  | None -> true
  | Some p1 ->
    match monotonic_shortest_path g s t with
    | None -> false
    | Some p2 ->
      let w1 = get_path_weight g p1 in
      let w2 = get_path_weight g p2 in
      w2 <= w1

(*  main testing function  *)
let test_monotonic_shortest_path g =
  let all_nodes = get_nodes g in
  List.iter (
      fun s ->
      (List.iter
         (fun t ->
           assert (test_path_monotonic g s t);
           assert (test_path_connected g s t);
           assert (test_that_is_path_graph g s t);
           assert (test_monotonically_reachable_hence_has_path g s t);
           assert (test_shortest_is_shorter g s t);
         )
      ) all_nodes
    ) all_nodes;
  true

(* tester *)
let tester_monotonic _ =
  test_monotonic_shortest_path example_graph_bf &&
    test_monotonic_shortest_path example_graph_dijkstra

(*
open W13_q3
let%test "monotonic_shortest_path" = tester_monotonic ()
*)
