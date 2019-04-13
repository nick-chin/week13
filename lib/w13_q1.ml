open Week_01
open Week_06
open Week_08_HashTable
open Week_10_ReadingFiles
open Week_12_BST
open Week_12_Graphs

(* let get_exn o = match o with
 *   | Some e -> e
 *   | _ -> raise (Failure "Empty option!") 
 * 
 * 
 * module ListBasedStack = struct
 *     type 'e t = 'e list ref
 *     let mk_stack _ = ref []
 *     let is_empty s = match !s with
 *       | [] -> true
 *       | _ -> false
 *     let push s e = 
 *       let c = !s in
 *       s := e :: c
 *     let pop s = match !s with
 *       | h :: t ->
 *         s := t; Some h
 *       | _ -> None
 *   end
 * 
 * module DoublyLinkedList = 
 *   struct
 *     type 'e dll_node = {
 *       value : 'e ref;
 *       prev  : 'e dll_node option ref;
 *       next  : 'e dll_node option ref
 *     }
 *     type 'e t = 'e dll_node option
 * 
 *     let mk_node e = {
 *       value = ref e;
 *       prev = ref None;
 *       next = ref None
 *     }
 * 
 *     let prev n =  !(n.prev)
 *     let next n =  !(n.next)
 *     let value n = !(n.value)
 *     let set_value n v = n.value := v
 * 
 *     let insert_after n1 n2 = 
 *       let n3 = next n1 in
 *       (match n3 with 
 *        | Some n -> n.prev := Some n2
 *        | _ -> ());
 *       n2.next := n3;
 *       n1.next := Some n2;
 *       n2.prev := Some n1
 * 
 *     let insert_before n1 n2 = 
 *       let n0 = prev n2 in
 *       (match n0 with 
 *        | Some n -> n.next := Some n1
 *        | _ -> ());
 *       n1.prev := n0;
 *       n1.next := Some n2;
 *       n2.prev := Some n1
 * 
 *     let rec move_to_head n = 
 *       match prev n with
 *       | None -> None
 *       | Some m -> move_to_head m
 *       
 *     let to_list_from n = 
 *       let res = ref [] in
 *       let iter = ref (Some n) in
 *       while !iter <> None do
 *         let node = (get_exn !iter) in
 *         res := (value node) :: ! res;
 *         iter := next node  
 *       done;
 *       List.rev !res
 * 
 *     let remove n = 
 *       (match prev n with
 *       | None -> ()
 *       | Some p -> p.next := next n);
 *       (match next n with
 *       | None -> ()
 *       | Some nxt -> nxt.prev := prev n);
 * 
 *   end 
 * 
 * 
 * module DLLBasedQueue = struct
 *   open DoublyLinkedList
 *     
 *     type 'e t = {
 *       head : 'e dll_node option ref;
 *       tail : 'e dll_node option ref;
 *     }
 * 
 *     (\* Tell about aliasing! *\)
 *     let mk_queue _sz = 
 *       {head = ref None; 
 *        tail = ref None}
 *     
 *     let is_empty q = 
 *       !(q.head) = None
 *       
 *     let is_full _q = false
 *       
 *     let enqueue q e = 
 *       let n = mk_node e in
 *       (\* Set the head *\)
 *       (if !(q.head) = None
 *        then q.head := Some n);
 *       (\* Extend the tail *\)
 *       (match !(q.tail) with
 *        | Some t -> insert_after t n;
 *        | None -> ());
 *       q.tail := Some n 
 * 
 *     let dequeue q =
 *       match !(q.head) with
 *       | None -> None
 *       | Some n -> 
 *         let nxt = next n in
 *         q.head := nxt;
 *         remove n; (\* This is not necessary *\)
 *         Some (value n)
 * 
 *     let queue_to_list q = match !(q.head) with
 *       | None -> []
 *       | Some n -> to_list_from n
 * 
 *   end
 * 
 * 
 * 
 * module type HashTable = sig
 *   type key
 *   type 'v hash_table
 *   val mk_new_table : int -> (key * 'v) hash_table 
 *   val insert : (key * 'v) hash_table -> key -> 'v -> unit
 *   val get : (key * 'v) hash_table -> key -> 'v option
 *   val remove : (key * 'v) hash_table -> key -> unit
 *   val print_hash_table : 
 *     (key -> string) ->
 *     ('v -> string) ->
 *     (key * 'v) hash_table -> unit
 * end
 * 
 * (\* 4. Exposing keys *\)
 * 
 * module type KeyType = sig
 *   type t
 * end
 * 
 * (\* 5. Redefining our hash-table *\)
 * 
 * (\* Redefining our hash-table *\)
 * module SimpleListBasedHashTable(K: KeyType) = struct
 *   type key = K.t
 * 
 *   type 'v hash_table = {
 *     buckets : 'v list array;
 *     capacity : int; 
 *   }
 * 
 *   let mk_new_table cap = 
 *     let buckets = Array.make cap [] in
 *     {buckets = buckets;
 *      capacity = cap}
 *   
 *   let insert ht k v = 
 *     let hs = Hashtbl.hash k in
 *     let bnum = hs mod ht.capacity in 
 *     let bucket = ht.buckets.(bnum) in
 *     let clean_bucket = 
 *       List.filter (fun (k', _) -> k' <> k) bucket in
 *     ht.buckets.(bnum) <- (k, v) :: clean_bucket
 * 
 *   let get ht k = 
 *     let hs = Hashtbl.hash k in
 *     let bnum = hs mod ht.capacity in 
 *     let bucket = ht.buckets.(bnum) in
 *     let res = List.find_opt (fun (k', _) -> k' = k) bucket in
 *     match res with 
 *     | Some (_, v) -> Some v
 *     | _ -> None
 * 
 *   (\* Slow remove - introduce for completeness *\)
 *   let remove ht k = 
 *     let hs = Hashtbl.hash k in
 *     let bnum = hs mod ht.capacity in 
 *     let bucket = ht.buckets.(bnum) in
 *     let clean_bucket = 
 *       List.filter (fun (k', _) -> k' <> k) bucket in
 *     ht.buckets.(bnum) <- clean_bucket
 * 
 *   let print_hash_table ppk ppv ht = 
 *     let open Printf in
 *     print_endline @@ sprintf "Capacity: %d" (ht.capacity);
 *     print_endline "Buckets:";
 *     let buckets = (ht.buckets) in
 *     for i = 0 to (ht.capacity) - 1 do
 *       let bucket = buckets.(i) in
 *       if bucket <> [] then (
 *         (\* Print bucket *\)
 *         let s = List.fold_left 
 *             (fun acc (k, v) -> acc ^ (sprintf "(%s, %s); ") (ppk k) (ppv v)) "" bucket in
 *         printf "%d -> [ %s]\n" i s)
 *     done
 * end 
 * 
 * (\*************************\)
 * (\* Resizeable hash table *\)
 * (\*************************\)
 * 
 * module ResizableListBasedHashTable(K : KeyType) = struct
 *   type key = K.t
 * 
 *   type 'v hash_table = {
 *     buckets : 'v list array ref;
 *     size : int ref; 
 *     capacity : int ref; 
 *   }
 * 
 *   let mk_new_table cap = 
 *     let buckets = Array.make cap [] in
 *     {buckets = ref buckets;
 *      capacity = ref cap;
 *      size = ref 0}
 * 
 *   let rec insert ht k v = 
 *     let hs = Hashtbl.hash k in
 *     let bnum = hs mod !(ht.capacity) in 
 *     let bucket = !(ht.buckets).(bnum) in
 *     let clean_bucket = 
 *       List.filter (fun (k', _) -> k' <> k) bucket in
 *     let new_bucket = (k, v) :: clean_bucket in
 *     !(ht.buckets).(bnum) <- new_bucket;
 *     (\* Increase size *\)
 *     (if List.length bucket < List.length new_bucket
 *     then ht.size := !(ht.size) + 1);
 *     (\* Resize *\)
 *     if !(ht.size) > !(ht.capacity) + 1
 *     then resize_and_copy ht
 * 
 *   and resize_and_copy ht =
 *     let new_capacity = !(ht.capacity) * 2 in
 *     let new_buckets = Array.make new_capacity [] in
 *     let new_ht = {
 *       buckets = ref new_buckets;
 *       capacity = ref new_capacity;
 *       size = ref 0;
 *     } in
 *     let old_buckets = !(ht.buckets) in
 *     let len = Array.length old_buckets in 
 *     for i = 0 to len - 1 do
 *       let bucket = old_buckets.(i) in
 *       List.iter (fun (k, v) -> insert new_ht k v) bucket
 *     done;
 *     ht.buckets := !(new_ht.buckets);
 *     ht.capacity := !(new_ht.capacity);
 *     ht.size := !(new_ht.size)
 *       
 *      
 *   let get ht k = 
 *     let hs = Hashtbl.hash k in
 *     let bnum = hs mod !(ht.capacity) in 
 *     let bucket = !(ht.buckets).(bnum) in
 *     let res = List.find_opt (fun (k', _) -> k' = k) bucket in
 *     match res with 
 *     | Some (_, v) -> Some v
 *     | _ -> None
 * 
 *   (\* Slow remove - introduce for completeness *\)
 *   let remove ht k = 
 *     let hs = Hashtbl.hash k in
 *     let bnum = hs mod !(ht.capacity) in 
 *     let bucket = !(ht.buckets).(bnum) in
 *     let clean_bucket = 
 *       List.filter (fun (k', _) -> k' <> k) bucket in
 *     !(ht.buckets).(bnum) <- clean_bucket;
 *     (if List.length bucket > List.length clean_bucket
 *     then ht.size := !(ht.size) - 1);
 *     assert (!(ht.size) >= 0)
 * 
 * 
 *   let print_hash_table ppk ppv ht = 
 *     let open Printf in
 *     print_endline @@ sprintf "Capacity: %d" !(ht.capacity);
 *     print_endline @@ sprintf "Size:     %d" !(ht.size);
 *     print_endline "Buckets:";
 *     let buckets = !(ht.buckets) in
 *     for i = 0 to !(ht.capacity) - 1 do
 *       let bucket = buckets.(i) in
 *       if bucket <> [] then (
 *         (\* Print bucket *\)
 *         let s = List.fold_left 
 *             (fun acc (k, v) -> acc ^ (sprintf "(%s, %s); ") (ppk k) (ppv v)) "" bucket in
 *         printf "%d -> [ %s]\n" i s)
 *     done
 *       
 * end 
 * 
 * 
 * (\* -----------Week 10-------------------- *\)
 * 
 *                                                 
 * open Core_kernel
 * 
 * (\* Reading *\)
 * 
 * let read_file_to_strings filename = 
 *   let file = In_channel.create filename in
 *   let strings = In_channel.input_lines file in
 *   In_channel.close file;
 *   strings
 * 
 * let read_file_to_single_string filename = 
 *   In_channel.with_file ~f:(fun input ->
 *       In_channel.input_all input) filename 
 * 
 * (\* Writing *\)
 * 
 * let write_string_to_file filename text = 
 *   let outc = Out_channel.create ~append:false ~fail_if_exists:false filename in
 *   Out_channel.output_string outc text;
 *   Out_channel.close outc
 * 
 * let write_strings_to_file filename lines = 
 *   Out_channel.with_file ~append:false ~fail_if_exists:false
 *     filename ~f:(fun out -> List.iter lines ~f:(fun s -> Out_channel.fprintf out "%s\r\n" s))
 * 
 * (\* Let's copy a file to another *\)
 * 
 * let copy_file old_file new_file = 
 *   let contents = read_file_to_single_string old_file in
 *   write_string_to_file new_file contents
 * 
 * 
 * module BinarySearchTree = struct
 * 
 * (\*
 * Supported operations:
 * * Get minimum
 * * Get maximum
 * * Find an element
 * * Find a successor
 * * Insert an element
 * * Delete an element
 * *\)
 * 
 *   (\**********************************\)
 *   (\*    1.  Defining a tree         *\)
 *   (\**********************************\)
 * 
 *   type 'e tree_node = {
 *     value : 'e;
 *     parent  : 'e tree_node option ref;
 *     left  : 'e tree_node option ref;
 *     right  : 'e tree_node option ref;
 *   }
 * 
 *   type 'e tree = {
 *     root : 'e tree_node option ref;
 *     size : int ref
 *   }
 * 
 *   let left n = !(n.left)
 *   let right n = !(n.right)
 *   let parent n = !(n.parent)
 *   let get_root t = !(t.root)
 *   let get_size t = !(t.size)
 * 
 *   let mk_node e = 
 *     {value = e;
 *      parent = ref None;
 *      left = ref None;
 *      right = ref None}
 *     
 *   let mk_tree _ = {root = ref None; size = ref 0}    
 *     
 *   let map_option o f z = match o with
 *     | None -> z
 *     | Some n -> f n
 *       
 * 
 *   (\**********************************\)
 *   (\*     2. Growing the tree        *\)
 *   (\**********************************\)
 *     
 *   let insert t e =       
 *     let rec insert_element n e = 
 *       let m = mk_node e in
 *       if e < n.value
 *       then match left n with
 *         | Some m -> insert_element m e
 *         | None ->
 *           m.parent := Some n;
 *           n.left := Some m;
 *           true
 *       else if e > n.value
 *       then match right n with
 *         | Some m -> insert_element m e
 *         | None ->
 *           m.parent := Some n;
 *           n.right := Some m;
 *           true
 *       else false
 *     in
 *     match !(t.root) with
 *     | None -> (
 *         t.root := Some (mk_node e);
 *         t.size := 1;
 *         true)
 *     | Some n -> 
 *       if insert_element n e
 *       then (t.size := !(t.size) + 1; true)
 *       else false
 *                      
 *               
 *   (\**********************************\)
 *   (\*     2.5 Tree invariant         *\)
 *   (\**********************************\)
 * 
 *   let check_bst_inv t = 
 *     let rec walk node p = 
 *       (p node.value) &&
 *       let res_left = match left node with
 *         | None -> true
 *         | Some l -> walk l (fun w -> p w && w <= node.value)
 *       in
 *       let res_right = match right node with
 *         | None -> true
 *         | Some r -> walk r (fun w -> p w && w >= node.value)
 *       in
 *       res_left && res_right
 *     in
 *     match !(t.root) with
 *     | None -> true
 *     | Some n -> walk n (fun _ -> true)
 * 
 *   (\**********************************\)
 *   (\*     3. Printing a tree         *\)
 *   (\**********************************\)
 * 
 *   let print_tree pp snum t = 
 *     let print_node_with_spaces l s = 
 *       for i = 0 to s - 1 do 
 *         Printf.printf " "
 *       done;
 *       print_endline (pp l.value);
 *     in
 * 
 *     let rec walk s node = match node with
 *       | None -> ()
 *       | Some n -> begin
 *           walk (s + snum) (right n);
 *           print_node_with_spaces n s;
 *           walk (s + snum) (left n);
 *         end      
 *     in
 *     map_option (get_root t) (fun n -> walk 0 (Some n)) ()
 *     
 *   (\**********************************\)
 *   (\*     4. Exploring the tree      *\)
 *   (\**********************************\)
 * 
 *   let search t k = 
 *     let rec walk k n = 
 *       let nk = n.value in 
 *       if k = nk then Some n
 *       else if k < nk
 *       then match left n with
 *         | None -> None
 *         | Some l -> walk k l
 *       else match right n with
 *         | None -> None
 *         | Some r -> walk k r
 *     in
 *     map_option (get_root t) (walk k) None
 * 
 *   (\**********************************\)
 *   (\* 5. Traversing a tree with DFS  *\)
 *   (\**********************************\)
 * 
 *   open DLLBasedQueue
 * 
 *   let depth_first_search_rec t = 
 *     let rec walk q n =
 *       enqueue q n.value;
 *       (match left n with
 *        | Some l -> walk q l
 *        | None -> ());
 *       (match right n with
 *        | Some r -> walk q r
 *        | None -> ());
 *     in
 *     let acc = (mk_queue 0) in
 *     map_option (get_root t) (walk acc) ();
 *     queue_to_list acc
 * 
 *   let depth_first_search_loop t = 
 *     let open ListBasedStack in
 *     let loop stack q =
 *       while not (is_empty stack) do
 *         let n = get_exn @@ pop stack in
 *         enqueue q n.value;
 *         (match right n with
 *          | Some r -> push stack r
 *          | _ -> ());
 *         (match left n with
 *          | Some l -> push stack l
 *          | _ -> ());
 *       done
 *     in
 *     let acc = (mk_queue 0) in
 *     let stack = mk_stack 0 in
 *     (match get_root t with
 *     | None -> ()
 *     | Some n -> begin
 *         push stack n;
 *         loop stack acc;
 *       end);      
 *     queue_to_list acc
 * 
 *   (\**********************************\)
 *   (\* 6. Traversing a tree with BFS  *\)
 *   (\**********************************\)
 * 
 *   let breadth_first_search_loop t = 
 *     let loop wlist q depth =
 *       while not (is_empty wlist) do
 *         let n = get_exn @@ dequeue wlist in
 *         enqueue q n.value;
 *         (match left n with
 *          | Some l -> enqueue wlist l
 *          | _ -> ());
 *         (match right n with
 *          | Some r -> enqueue wlist r
 *          | _ -> ());
 *       done
 *     in
 *     let acc = (mk_queue 0) in
 *     let wlist = mk_queue 0 in
 *     (match get_root t with
 *     | None -> ()
 *     | Some n -> begin
 *         enqueue wlist n;
 *         loop wlist acc 0;
 *       end);      
 *     queue_to_list acc
 * 
 *   let elements t = breadth_first_search_loop t
 * 
 *   (\**********************************\)
 *   (\* 7.  Finding a minimum node     *\)
 *   (\**********************************\)
 * 
 *   let rec find_min_node n = 
 *     match left n with
 *     | Some m -> find_min_node m
 *     | None -> n
 * 
 *   (\* Question: how to find a successor of a node in a tree? *\)
 * 
 *   (\**********************************\)
 *   (\* 8.    Deletion of an element   *\)
 *   (\**********************************\)
 * 
 *   (\* Replacing node U by (optional) node V in T. *\)
 *   let transplant t u v = 
 *     (match parent u with
 *     | None -> t.root := v
 *     | Some p -> 
 *       match left p with
 *       | Some l when u == l -> p.left := v
 *       | _ -> p.right := v);
 *     (\* Update parent of v *\)
 *     match v with 
 *     | Some n -> n.parent := parent u
 *     | _ -> ()
 * 
 *   (\* Deleting the a node z from tree *\)
 *   (\* z must be in the tree *\)
 *   let delete_node t z = 
 *     t.size := !(t.size) - 1;
 *     if left z = None
 *     then transplant t z (right z)
 *     else if right z = None
 *     then transplant t z (left z)
 *     else
 *       (\* Finding the successor of `z` *\)
 *       let z_right_child = (get_exn @@ right z) in
 *       let y = find_min_node z_right_child in
 *       (\* Fact: `y` has no left child *\)
 * 
 *       (if parent y <> None &&
 *           z != get_exn @@ parent y
 *        then 
 *       (\*  If y is not immediately under z,
 *           replace y by its right subtree *\)
 *          let x = right y in
 *          (transplant t y x;
 *           y.right := right z;
 *           (get_exn @@ right y).parent := Some y));
 * 
 *       (\* Now `y` replaces `z` at its position *\)
 *       transplant t z (Some y);
 *       y.left := !(z.left);
 *       (get_exn @@ left y).parent := Some y
 *   
 * 
 *   (\**********************************\)
 *   (\* 9. Rotations and balanced tree *\)
 *   (\**********************************\)
 * 
 *   let left_rotate t x =
 *     match right x with
 *     | None -> ()
 *     | Some y ->
 *       
 *       (\* turn y's left subtree into x's right subtree *\)
 *       x.right := left y;
 *       (if left y <> None
 *        then (get_exn @@ left y).parent := Some x);
 *       
 *       (\* link x's parent to y *\)
 *       y.parent := parent x;
 * 
 *       (match parent x with 
 *        | None -> t.root := Some y
 *        | Some p -> match left p with
 *          | Some l when x == l ->
 *            p.left := Some y
 *          | _ ->
 *            p.right := Some y);
 *             
 *       (\* Make x the left child of y *\)
 *       y.left := Some x;
 *       x.parent := Some y      
 *       
 * end
 * 
 * 
 *    
 * module AdjacencyGraphs = struct 
 * 
 *   type ('a, 'b) graph = {
 *     size : int;
 *     adj : int list array;
 *     node_payloads : (int * 'a) list ref;
 *     edge_labels : ((int * int) * 'b) list ref
 *   }
 * 
 *   let mk_graph n = {
 *     size = n;
 *     adj = Array.make n [];
 *     node_payloads = ref [];
 *     edge_labels = ref [];
 *   }
 * 
 *   let in_range g node = 
 *     node >= 0 && node < g.size
 * 
 *   let set_payload g node p = 
 *     assert (in_range g node);
 *     let pl = !(g.node_payloads) |>
 *              List.filter (fun (n, _) -> n <> node) in
 *     g.node_payloads := (node, p) :: pl
 * 
 *   
 *   (\*******************************************\)
 *   (\*            Working with Edges           *\)                           
 *   (\*******************************************\)
 * 
 *   let add_edge g src dst = 
 *     assert (in_range g src && in_range g dst);
 *     let out = g.adj.(src) in
 *     let out' = List.filter (fun n -> n <> dst) out in
 *     g.adj.(src) <- dst :: out'
 * 
 *   let remove_edge g src dst = 
 *     assert (in_range g src && in_range g dst);
 *     let out = g.adj.(src) in
 *     let out' = List.filter (fun n -> n <> dst) out in
 *     g.adj.(src) <- out'
 *     
 *   let set_edge_label g src dst l = 
 *     assert (in_range g src && in_range g dst);
 *     let labs = !(g.edge_labels) in
 *     let labs' = List.filter (fun ((s, d), _) -> (s, d) <> (src, dst)) labs in
 *     g.edge_labels := ((src, dst), l) :: labs'
 * 
 *   (\* Get all edges as pairs *\)
 *   let edges g = 
 *     let open DLLBasedQueue in
 *     let q = mk_queue g.size in
 *     for i = 0 to g.size - 1 do
 *       let next = g.adj.(i) in
 *       List.iter (fun n -> enqueue q (i, n)) next
 *     done;
 *     queue_to_list q
 * 
 * end 
 * 
 * (\*******************************************\)
 * (\*    Rendering the graph via GraphViz     *\)                           
 * (\*******************************************\)
 * 
 * (\* let graphviz_string_of_graph gtyp conn vattrib eattrib g = 
 *  *   let preamble = gtyp ^ " {\n" in
 *  *   let epilogue = "}" in
 *  *   let body = 
 *  *     AdjacencyGraphs.edges g |>
 *  *     List.map (fun (s, d) -> 
 *  *         Printf.sprintf "%s %s %s %s" 
 *  *           (vattrib s) conn (vattrib d) (eattrib (s, d))) |>
 *  *     String.concat ";\n" in
 *  *   preamble ^ body ^ epilogue
 *  *   
 *  * let graphviz_no_payload g out = 
 *  *   let s = graphviz_string_of_graph "digraph" " -> " 
 *  *       string_of_int (fun _ -> "") g in
 *  *   write_string_to_file out s *\)
 * 
 * 
 * (\*
 * Rendering via GraphViz:
 * dot -Tpdf filename.dot -o outfile.pdf
 * for instance:
 * dot -Tpdf simple.dot -o simple.pdf
 * *\)
 * 
 * (\*
 * Question: which operations are not so good for simple graphs?
 * *\)
 * 
 * (\*
 * Exercise: 
 * * Implement a procedure for random generation of graphs.
 * * Implement a procedure for random generation of connected graphs.
 * *\)
 * 
 * (\**************************************************************\)
 * (\*        Mutable graphs as a linked data structures          *\)
 * (\**************************************************************\)
 * 
 * module LinkedGraphs = struct
 * 
 *   (\*************************************************\)
 *   (\*                     Nodes                     *\)
 *   (\*************************************************\)               
 *   
 *   type 'a node = {
 *     id : int;
 *     value : 'a ref;
 *     next : int list ref;
 *     prev : int list ref
 *   }
 * 
 *   let get_value n = !(n.value)
 *   let get_next n = !(n.next)
 *   let get_prev n = !(n.prev)
 * 
 *   let add_prev node src = 
 *     let prev' = get_prev node |>
 *                 List.filter (fun n -> n <> src) in
 *     node.prev := src :: prev'
 * 
 *   let add_next node dst = 
 *     let next' = get_next node |>
 *                 List.filter (fun n -> n <> dst) in
 *     node.next := dst :: next'
 * 
 *   (\*************************************************\)
 *   (\*           Auxiliary definitions               *\)
 *   (\*************************************************\)               
 * 
 *   (\* open BinarySearchTree
 *    * open ResizableListBasedHashTable *\)
 * 
 *   module Set = BinarySearchTree
 *   module NodeTable = 
 *     ResizableListBasedHashTable(struct type t = int end)
 *   module EdgeTable = 
 *     ResizableListBasedHashTable(struct type t = int * int end)
 * 
 *   type 'a set = 'a Set.tree
 *   
 *   (\*************************************************\)
 *   (\*                Working with Graphs            *\)    
 *   (\*************************************************\)
 *   
 *   type ('a, 'b) graph = {
 *     next_node_id : int ref;
 *     nodes : int set;
 *     node_map : (int * 'a node) NodeTable.hash_table;
 * 
 *     edges : (int * int) set;
 *     edge_labels : ((int * int) * 'b) EdgeTable.hash_table
 *   }
 * 
 *   (\**************************************************\)
 *   (\*                Querying the graph              *\)
 *   (\**************************************************\)
 * 
 *   (\* Graph size *\)
 *   let v_size g = !(g.next_node_id)
 *   let e_size g = BinarySearchTree.get_size g.edges
 *   let get_nodes g = Set.elements g.nodes
 * 
 *   (\* Refer to the node in the graph *\)
 *   let get_node g i = get_exn @@ NodeTable.get g.node_map i
 * 
 *   let get_succ g n = 
 *     let node = get_node g n in
 *     get_next node
 * 
 *   let get_prev g n = 
 *     let node = get_node g n in
 *     get_prev node
 * 
 *   let node_in_graph g n = 
 *     let nodes = g.nodes in
 *     Set.search nodes n <> None
 * 
 *   let edge_in_graph g src dst = 
 *     let nodes = g.edges in
 *     Set.search nodes (src, dst) <> None
 * 
 *   (\**************************************************\)
 *   (\*                Altering the graph              *\)
 *   (\**************************************************\)
 * 
 *   (\* Making a graph *\)
 *   let mk_graph _ = {
 *     next_node_id = ref 0;
 *     nodes = Set.mk_tree ();
 *     node_map = NodeTable.mk_new_table 10;
 *     edges = Set.mk_tree ();
 *     edge_labels = EdgeTable.mk_new_table 10
 *   }
 * 
 *   (\* Adding nodes *\)
 *   let add_node g v = 
 *     let new_id = !(g.next_node_id) in
 *     g.next_node_id := !(g.next_node_id) + 1;
 *     let node = {
 *       id = new_id;
 *       value = ref v;
 *       next = ref [];
 *       prev = ref [];
 *     } in
 *     (\* Register node *\)
 *     let _ = Set.insert g.nodes new_id in
 *     (\* Register node payload *\)
 *     NodeTable.insert g.node_map new_id node
 * 
 * 
 *   (\* Adding edges *\)
 *   let add_edge g src dst =
 *     assert (node_in_graph g src && node_in_graph g src);
 *     (\* Register edge *\)
 *     let _ = Set.insert g.edges (src, dst) in
 *     (\* Add information to individual nodes *\)
 *     let src_node = get_exn @@ NodeTable.get g.node_map src in
 *     let dst_node = get_exn @@ NodeTable.get g.node_map dst in
 *     add_prev dst_node src;
 *     add_next src_node dst 
 * 
 * 
 *   let set_edge_label g src dst l =
 *     assert (node_in_graph g src && node_in_graph g src);
 *     assert (edge_in_graph g src dst);
 *     (\* Register label *\)
 *     EdgeTable.insert g.edge_labels (src, dst) l
 * 
 * 
 *   (\*************************************************\)
 *   (\*        Switching between representations      *\)    
 *   (\*************************************************\)
 * 
 *   (\* No node payloads *\)
 *   let from_simple_adjacency_graph (ag : ('a, 'b) AdjacencyGraphs.graph) = 
 *     let g = mk_graph () in
 *     
 *     (\* Add nodes *\)
 *     for i = 0 to ag.size - 1 do
 *       let v = snd @@ List.find (fun (n, _) -> n = i) !(ag.node_payloads) in
 *       add_node g v;
 *     done;
 * 
 *     (\* Add edges *\)
 *     for i = 0 to ag.size - 1 do
 *       ag.adj.(i) |> 
 *       List.map (fun n -> (i, n)) |>
 *       List.iter (fun (src, dst) -> add_edge g src dst)
 *     done;
 * 
 *     (\* Add edge labels *\)
 *     List.iter (fun ((src, dst), l) -> set_edge_label g src dst l) 
 *       !(ag.edge_labels);
 * 
 *     g
 * 
 *       
 *   let to_adjacency_graph g = 
 *     let size = v_size g in
 *     let ag = AdjacencyGraphs.mk_graph size in
 * 
 *     (\* Set node payloads *\)
 *     Set.elements g.nodes |>
 *     List.iter (fun n -> 
 *         let node = get_exn @@ NodeTable.get g.node_map n in
 *         AdjacencyGraphs.set_payload ag n (get_value node));
 * 
 *     (\* Add edges *\)
 *     let edges = Set.elements g.edges in
 *     List.iter (fun (src, dst) -> AdjacencyGraphs.add_edge ag src dst) edges;
 * 
 *     (\* Add edges labels *\)
 *     List.iter (fun (s, d) ->
 *         match EdgeTable.get g.edge_labels (s, d) with
 *         | None -> ()
 *         | Some l -> AdjacencyGraphs.set_edge_label ag s d l) edges;
 *     ag
 *       
 * end
 * 
 * 
 * open LinkedGraphs
 * 
 *                     
 * (\*****************************************\)
 * (\*   0. Reading a graph with payloads    *\)
 * (\*****************************************\)
 * 
 * let read_graph_and_payloads size nvalue elist elabels = 
 *   let open AdjacencyGraphs in 
 *   let g = mk_graph size in
 *   for i = 0 to g.size - 1 do
 *     set_payload g i nvalue.(i) 
 *   done;  
 *   List.iter (fun (s, d) -> add_edge g s d) elist;
 *   List.iter (fun (s, d, l) -> set_edge_label g s d l) elabels;
 *   LinkedGraphs.from_simple_adjacency_graph g
 *   
 * 
 * (\*****************************************\)
 * (\*        1. Reachability queries        *\)
 * (\*****************************************\)
 * 
 * let reachable g init final = 
 *   let rec walk path visited n = 
 *     if n = final 
 *     then Some path
 *     else if List.mem n visited 
 *     then None
 *     else
 *       (\* Try successors *\)
 *       let node = get_node g n in
 *       let successors = get_next node in
 *       let visited' = n :: visited in
 *       let rec iter = function
 *         | [] -> None
 *         | h :: t -> 
 *           let path' = (n, h) :: path in
 *           match walk path' visited' h with
 *           | Some p -> Some p
 *           | None -> iter t
 *       in
 *       iter successors
 *   in
 *   match walk [] [] init with
 *   | Some p -> Some (List.rev p)
 *   | _ -> None
 * 
 * 
 * let is_reachable g init final = 
 *   reachable g init final <> None
 * 
 * (\* Graphical representation *\)
 * 
 * (\*
 * Taking attributes from_simple_adjacency_graph
 * https://graphs.grevian.org/example
 * *\)
 * 
 * let bold_edge = "[color=red,penwidth=3.0]"
 * 
 * (\* let graphviz_with_path g init final out = 
 *  *   let r = reachable g init final in 
 *  *   let attrib (s, d) = match r with
 *  *     | None -> ""
 *  *     | Some p -> 
 *  *       if List.mem (s, d) p 
 *  *       then bold_edge
 *  *       else ""
 *  *   in
 *  *   let open Week_10_ReadingFiles in
 *  *   let ag = LinkedGraphs.to_adjacency_graph g in
 *  *   let s = graphviz_string_of_graph "digraph" " -> " 
 *  *       string_of_int attrib ag in
 *  *   write_string_to_file out s *\)
 * 
 * 
 * (\*****************************************\)
 * (\*        2. Depth-first search          *\)
 * (\*****************************************\)
 * 
 * module GraphDFS = struct
 *   
 *   open NodeTable 
 * 
 *   type color = White | Gray | Black
 * 
 *   let rec dfs g = 
 *     let color_map = mk_new_table (v_size g) in
 *     let tree_map = mk_new_table (v_size g) in
 *     let time_map = mk_new_table (v_size g) in
 *     let has_cycles = ref false in
 *     let roots = ref [] in
 *     let all_nodes = get_nodes g in
 * 
 *     (\* Make all nodes white *\)
 *     List.iter (fun n -> insert color_map n White) all_nodes;
 *     (\* Insert all nodes to the tree *\)
 *     List.iter (fun n -> insert tree_map n []) all_nodes;
 * 
 *     let time = ref 0 in 
 * 
 * 
 *     let rec dfs_visit u = 
 *       time := !time + 1;
 *       let u_in = !time in
 *       insert color_map u Gray;
 *       get_succ g u |> List.iter (fun v -> 
 *           let v_color = get_exn @@ get color_map v in
 *           if v_color = White
 *           then begin
 *             let siblings = get_exn @@ get tree_map u in
 *             insert tree_map u (v :: siblings);
 *             dfs_visit v
 *           end 
 *           else if v_color = Gray 
 *           then has_cycles := true) ;
 *       insert color_map u Black;
 *       time := !time + 1;
 *       let u_out = !time in
 *       insert time_map u (u_in, u_out)
 *     in
 * 
 *     List.iter (fun n -> 
 *         if get_exn @@ get color_map n = White
 *         then begin
 *           (\* Record roots *\)
 *           roots := n :: !roots;
 *           dfs_visit n
 *         end) 
 *       all_nodes;
 * 
 *       (!roots, tree_map, time_map, !has_cycles)  
 * 
 *   (\* (\\* Visualise with DFS *\\)
 *    * let graphviz_with_dfs g out = 
 *    * let (_, tree, _, _) = dfs g in 
 *    * let eattrib (s, d) = match get tree s with
 *    *   | None -> ""
 *    *   | Some p -> 
 *    *     if List.mem d p 
 *    *     then bold_edge
 *    *     else ""
 *    * in
 *    * let open Week_10_ReadingFiles in
 *    * let ag = LinkedGraphs.to_adjacency_graph g in
 *    * let s = graphviz_string_of_graph "digraph" " -> " 
 *    *     string_of_int eattrib ag in
 *    * write_string_to_file out s *\)
 *   
 * 
 *   (\* DFS-induced search *\)
 *   let is_reachable_via_dfs g init final = 
 *     let (roots, tree, _, _) = dfs g in
 *     let rec walk n = 
 *       if n = final then true
 *       else 
 *         get tree n |> 
 *         get_exn |>
 *         List.exists (fun v -> walk v)
 *     in
 *     if List.mem init roots 
 *     then walk init
 *     else false
 * 
 *   (\* Question: is reachability equivalent to DFS-reachability *\)
 * 
 * end
 * 
 * (\*****************************************\)
 * (\*         4. Topological sort           *\)
 * (\*****************************************\)
 * 
 * (\* let graphviz_with_payload g values out = 
 *  *   let eattrib e = "" in
 *  *   let vattrib n = values.(n) in
 *  *   let open Week_10_ReadingFiles in
 *  *   let ag = LinkedGraphs.to_adjacency_graph g in
 *  *   let s = graphviz_string_of_graph "digraph" " -> " 
 *  *       vattrib eattrib ag in
 *  *   write_string_to_file out s *\)
 *     
 * module TopologicalSort = struct
 * 
 *   open NodeTable 
 * 
 *   let get_last_time m n = get_exn @@ get m n
 * 
 *   let topo_sort g = 
 *     let (_, _, time_map, _) = GraphDFS.dfs g in
 *     get_nodes g |>
 *     List.sort (fun n1 n2 ->
 *         let (_, t1) = get_last_time time_map n1 in
 *         let (_, t2) = get_last_time time_map n2 in
 *         if t1 < t2 then 1
 *         else if t1 > t2 then -1
 *         else 0)
 * 
 * end *)

                       
(*---------------------------------------------- *)

open LinkedGraphs
   

let pos_to_coord s =
  let x = int_of_char @@ String.get s 0 in
  let y = int_of_char @@ String.get s 1 in
  (x, y);;

let coord_to_node (x, y) =
  x - 97 + ((y - 48) * 8);;

let node_to_coord n =
  let x = (mod) n 8 in
  let y = n / 8 in
  (x + 97, y + 48);;

let coord_to_pos (x, y) =
  let alpha = Char.escaped @@ char_of_int x in
  let num = Char.escaped @@ char_of_int y in
  String.concat "" [alpha; num];;

let fill_array _ =
  let arr = Array.make 64 "" in
  for i = 0 to 63 do
    arr.(i) <- coord_to_pos @@ node_to_coord i
  done;
  arr;;

let a = fill_array();;

let up_left g cell =
  let (x, y) = node_to_coord cell in
  if x - 1 < 97 || y - 2 < 48
  then ()
  else
    let dest = coord_to_node (x - 1, y - 2) in
    add_edge g cell dest;;

let up_right g cell =
  let (x, y) = node_to_coord cell in
  if x + 1 > 104 || y - 2 < 48
  then ()
  else
    let dest = coord_to_node (x + 1, y - 2) in
    add_edge g cell dest;;

let left_up g cell =
  let (x, y) = node_to_coord cell in
  if x - 2 < 97 || y - 1 < 48
  then ()
  else
    let dest = coord_to_node (x - 2, y - 1) in
    add_edge g cell dest;;

let right_up g cell =
  let (x, y) = node_to_coord cell in
  if x + 2 > 104 || y - 1 < 48
  then ()
  else
    let dest = coord_to_node (x + 2, y - 1) in
    add_edge g cell dest;;

let left_down g cell =
  let (x, y) = node_to_coord cell in
  if x - 2 < 97 || y + 1 > 55
  then ()
  else
    let dest = coord_to_node (x - 2, y + 1) in
    add_edge g cell dest;;

let right_down g cell =
  let (x, y) = node_to_coord cell in
  if x + 2 > 104 || y + 1 > 55
  then ()
  else
    let dest = coord_to_node (x + 2, y + 1) in
    add_edge g cell dest;;

let down_left g cell =
  let (x, y) = node_to_coord cell in
  if x - 1 < 97 || y + 2 > 55
  then ()
  else
    let dest = coord_to_node (x - 1, y + 2) in
    add_edge g cell dest;;

let down_right g cell =
  let (x, y) = node_to_coord cell in
  if x + 1 > 104 || y + 2 > 55
  then ()
  else
    let dest = coord_to_node (x + 1, y + 2) in
    add_edge g cell dest;;

add_edge;;

let make_chess_board _ =
  let g = mk_graph () in
  for i = 0 to 63 do
    let cell = coord_to_pos @@ node_to_coord i in
    add_node g cell
  done;
  for j = 0 to 63 do
    up_left g j;
    up_right g j;
    left_up g j;
    right_up g j;
    left_down g j;
    right_down g j;
    down_left g j;
    down_right g j;
  done;
  g;;


