open Week_01
open Week_06
open Week_08_HashTable
open Week_10_ReadingFiles
open Week_12_BST
open Week_12_Graphs
open LinkedGraphs   

let pos_to_coord s =
  let x = int_of_char @@ String.get s 0 in
  let y = int_of_char @@ String.get s 1 in
  (x, y)

let coord_to_node (x, y) =
  x - 97 + ((y - 48) * 8)

let node_to_coord n =
  let x = (mod) n 8 in
  let y = n / 8 in
  (x + 97, y + 48)

let coord_to_pos (x, y) =
  let alpha = Char.escaped @@ char_of_int x in
  let num = Char.escaped @@ char_of_int y in
  String.concat "" [alpha; num]

let up_left g cell =
  let (x, y) = node_to_coord cell in
  if x - 1 < 97 || y - 2 < 48
  then ()
  else
    let dest = coord_to_node (x - 1, y - 2) in
    add_edge g cell dest

let up_right g cell =
  let (x, y) = node_to_coord cell in
  if x + 1 > 104 || y - 2 < 48
  then ()
  else
    let dest = coord_to_node (x + 1, y - 2) in
    add_edge g cell dest

let left_up g cell =
  let (x, y) = node_to_coord cell in
  if x - 2 < 97 || y - 1 < 48
  then ()
  else
    let dest = coord_to_node (x - 2, y - 1) in
    add_edge g cell dest

let right_up g cell =
  let (x, y) = node_to_coord cell in
  if x + 2 > 104 || y - 1 < 48
  then ()
  else
    let dest = coord_to_node (x + 2, y - 1) in
    add_edge g cell dest

let left_down g cell =
  let (x, y) = node_to_coord cell in
  if x - 2 < 97 || y + 1 > 55
  then ()
  else
    let dest = coord_to_node (x - 2, y + 1) in
    add_edge g cell dest

let right_down g cell =
  let (x, y) = node_to_coord cell in
  if x + 2 > 104 || y + 1 > 55
  then ()
  else
    let dest = coord_to_node (x + 2, y + 1) in
    add_edge g cell dest

let down_left g cell =
  let (x, y) = node_to_coord cell in
  if x - 1 < 97 || y + 2 > 55
  then ()
  else
    let dest = coord_to_node (x - 1, y + 2) in
    add_edge g cell dest

let down_right g cell =
  let (x, y) = node_to_coord cell in
  if x + 1 > 104 || y + 2 > 55
  then ()
  else
    let dest = coord_to_node (x + 1, y + 2) in
    add_edge g cell dest

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
  g

let reachable_knight b pos1 pos2 =
  let final = coord_to_node @@ pos_to_coord pos2 in
  let rec walk path visited n =
    if n = final
    then Some path
    else if List.mem n visited
    then None
    else
      (* Try successors *)
      let node = get_node b n in
      let successors = get_next node in
      let visited' = n :: visited in
      let rec iter = function
        | [] -> None
        | h :: t ->
           let path' = (n, h) :: path in
           match walk path' visited' h with
           | Some p -> Some p
           | None -> iter t
      in
      iter successors
  in
  let init = coord_to_node @@ pos_to_coord pos1 in
  match walk [] [] init with
  | Some p ->
     Some (List.map (fun (x, y) ->
               (coord_to_pos @@ node_to_coord x, coord_to_pos @@ node_to_coord y))
             (List.rev p))
  | _ -> None

let is_reachable_knight b init final =
  reachable_knight b init final <> None

let print_edge_lists ls =
  Printf.printf "[";
  let rec printing lst =
    match lst with
    | [] -> Printf.printf "]\n";
    | (x, y) :: t ->
       Printf.printf "(%s, %s); " x y;
       printing t
  in
  printing ls
  
let random_knight_path _ =
  let b = make_chess_board () in
  let pos1 = coord_to_pos @@ node_to_coord @@ Random.int 64 in
  let pos2 = coord_to_pos @@ node_to_coord @@ Random.int 64 in
  assert (is_reachable_knight b pos1 pos2);
  let path = get_exn @@ reachable_knight b pos1 pos2 in
  Printf.printf "From %s to %s\n" pos1 pos2;
  Printf.printf "Path:\n";
  print_edge_lists path
