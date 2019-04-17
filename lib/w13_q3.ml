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

(* find increasing shortest path from node init to node final *)
let increasing_shortest_path g init final =
  let w = get_linked_edge_label g in
  let rec walk path visited n = 
    if n = final
    then Some path
    else if List.mem n visited 
    then None
    else
      let successors =  get_succ g n in
      let weights = List.map (fun e -> w n e) successors in
      (* sort successors by weight *)
      let successors = sort_l1_by_l2 successors weights in
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

(* find decreasing shortest path from node init to node final *)
let decreasing_shortest_path g init final =
  let w = get_linked_edge_label g in
  let rec walk path visited n = 
    if n = final
    then Some path
    else if List.mem n visited 
    then None
    else
      let successors =  get_succ g n in
      let weights = List.map (fun e -> w n e) successors in
      (* sort successors by weight *)
      let successors = sort_l1_by_l2 successors weights in
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

(* find monotonic shortest path from node s to node t *)
let monotonic_shortest_path g s t =
  let increasing_path = increasing_shortest_path g s t in
  let decreasing_path = decreasing_shortest_path g s t in
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
      else Some path1;;

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

(*****************************************)
(*             Random tests              *)
(*****************************************)

(* generate random graphs *)
let random_nodes vNum =
  Array.init vNum (fun i -> Char.escaped @@ char_of_int ((Random.int 26) + 97))

let remove_duplicate l =
  let comp x y =
    if fst x < fst y then -1
    else if fst x > fst y then 1
    else
      if snd x < snd y then -1
      else if snd x > snd y then 1
      else 0
  in
  let sorted = List.sort comp l in
  let rec walk l' acc =
    match l' with
    | h1 :: h2 :: t ->
       if h1 = h2 then walk t (h1 :: acc)
       else walk t (h2 :: h1 :: acc)
    | [h] -> h :: acc
    | _ -> acc
  in
  walk sorted []

(* generate random edges for a graph with vNum vertices and eNum edges *)
let random_edges vNum eNum =
  let es' = List.map (fun i -> (Random.int vNum, Random.int vNum)) (iota (eNum - 1)) in
  let es = List.filter (fun (x, y) -> x <> y) es' in
  (* remove duplicate edges *)
  remove_duplicate es

(* generate random weights for a list of edges *)
let random_weights es =
  let weights = List.map (fun i -> Random.int 10) es in
  weights

(* generate random labels *)
let random_labels es =
  let weights = random_weights es in
  let zipped = list_zip es weights in
  let labels = List.map (fun e -> (fst (fst e), snd (fst e), snd e)) zipped in
  labels

(* generate random directed graphs *)
let random_digraph vNum eNum =
  let nodes = random_nodes vNum in
  let es = random_edges vNum eNum in
  let labels = random_labels es in
  read_graph_and_payloads vNum nodes es labels

(* tests on 20 random graphs *)
let test_random_monotonic_shortest_path vNum eNum =
  let results = ref [] in
  for _ = 1 to 20 do
    let g = random_digraph vNum eNum in
    let res = test_monotonic_shortest_path g in
    results := res :: !results
  done;
  List.for_all (fun e -> e)  !results

(* random tester *)
let tester_monotonic_random _ =
  test_random_monotonic_shortest_path 5 20

(*
open W13_q3
let%test "monotonic_shortest_path" = tester_monotonic ()
let%test "monotonic_shortest_path_random" = tester_monotonic_random ()
*)
