open OUnit2
open Suffix_tree

let test_lcp test_ctxt =
  let s1 = "abc1" in
  let s2 = "abc2" in
  assert_equal 3 (ST.lcp s1 s2)

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
    [ "test_lcp" >:: test_lcp;
      "test_suffixes" >:: test_suffixes
    ]

let () =
  run_test_tt_main suite
