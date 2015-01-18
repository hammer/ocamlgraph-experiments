open OUnit2
open Suffix_tree.ST

let create_depth_two_st () =
  let g = G.create () in
  let root = G.V.create () in
  G.add_vertex g root;
  let leaves = Array.make 2 (G.V.create ()) in
  let st = { g; root; leaves } in
  let inner_node = G.V.create () in
  let e1 = G.E.create st.root "he" inner_node in
  G.add_edge_e st.g e1;
  let leaf = G.V.create () in
  let e2 = G.E.create inner_node "y there" leaf in
  G.add_edge_e st.g e2;
  st.leaves.(0) <- leaf;
  st

let test_lcp test_ctxt =
  let s1 = "abc1" in
  let s2 = "abc2" in
  assert_equal 3 (lcp s1 s2)

let test_lcp_path test_ctxt =
  let st = create_depth_two_st () in
  let e2 = List.hd (G.pred_e st.g st.leaves.(0)) in
  let es = G.succ_e st.g st.root in
  let s_len, e_info = lcp_path st "hey t" es in
  assert_equal s_len 5;
  let expected_e_info:split_info option = Some {e=e2; l_pre=3} in
  assert_equal e_info expected_e_info

let test_get_suffix test_ctxt =
  let st = create_depth_two_st () in
  assert_equal "hey there" (get_suffix st 0)

let test_suffixes test_ctxt =
  let s = "xabxa$" in
  let s_length = String.length s in
  let st = create s in
  for i = 0 to s_length - 1 do
    let st_suffix = get_suffix st i in
    let suffix = String.sub_exn s i (s_length - i) in
    assert_equal suffix st_suffix
  done

let suite =
  "suite" >:::
    [
      "test_lcp" >:: test_lcp;
      "test_lcp_path" >:: test_lcp_path;
      "test_get_suffix" >:: test_get_suffix;
      "test_suffixes" >:: test_suffixes
    ]

let () =
  run_test_tt_main suite
