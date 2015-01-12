open OUnit2
open Suffix_tree

let test_simple_st test_ctxt =
  let g = ST.create () in
  let v1 = ST.V.create (-1) in
  let v2 = ST.V.create 0 in
  let e1 = ST.E.create v1 "hey hey hey" v2 in
  ST.add_edge_e g e1;
  let s = ST.fold_pred_e (fun e s -> (ST.E.label e) ^ s) g v2 "" in
  assert_equal "hey hey hey" s
let suite =
  "suite" >:::
    [ "test_simple_st" >:: test_simple_st
    ]

let () =
  run_test_tt_main suite
