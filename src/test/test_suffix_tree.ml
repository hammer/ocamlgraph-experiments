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

let test_lcp_path test_ctxt =
  let g = ST.G.create () in
  let root = ST.G.V.create () in
  ST.G.add_vertex g root;
  let leaves = Array.make 1 (ST.G.V.create ()) in
  let st:ST.t = { g; root; leaves } in
  let leaf = ST.G.V.create () in
  let e = ST.G.E.create st.root "hey" leaf in
  ST.G.add_edge_e st.g e;
  st.leaves.(0) <- leaf;
  let es = ST.G.succ_e st.g st.root in
  let s_len, e_info = ST.lcp_path st "he" es in
  assert_equal s_len 2;
  let expected_e_info:ST.split_info option = Some {e; l_pre = 2} in
  assert_equal e_info expected_e_info

let suite =
  "suite" >:::
    [ "test_lcp" >:: test_lcp;
      "test_suffixes" >:: test_suffixes;
      "test_lcp_path" >:: test_lcp_path
    ]

let () =
  run_test_tt_main suite
