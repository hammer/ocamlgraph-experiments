open OUnit2
open Suffix_tree

let test_suffixes test_ctxt =
  let s = "hey hey hey" in
  let s_length = String.length s in
  let st = ST.create s in
  for i = 0 to s_length - 1 do
    let st_suffix = ST.get_suffix st i in
    let suffix = String.sub s i (s_length - i) in
    assert_equal suffix st_suffix
  done

let suite =
  "suite" >:::
    [ "test_suffixes" >:: test_suffixes
    ]

let () =
  run_test_tt_main suite
