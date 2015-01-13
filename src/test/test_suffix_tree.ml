open OUnit2
open Suffix_tree

let test_simple_st test_ctxt =
  let s = "hey hey hey" in
  let st = ST.create s in
  let derived_s = ST.get_suffix st 0 in
  assert_equal s derived_s

let suite =
  "suite" >:::
    [ "test_simple_st" >:: test_simple_st
    ]

let () =
  run_test_tt_main suite
