open W13_q2

let%test "Random Knight Path" =
  random_knight_path ();
  true


open W13_q5

let%test "Testing Random Spanning Tree" =
  let g = example_graph_undirected in
  let n = ref 100 in
  let counter = ref true in
  while !n <> 0 do
    counter := !counter && test_rst_size g && test_rst_connectness g && test_rst_connectness g && test_compare_weight_rst_mst g;
    n := !n - 1;
  done;
  !counter
