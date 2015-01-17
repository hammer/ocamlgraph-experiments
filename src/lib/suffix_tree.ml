module ST = struct
  module String = struct include Sosa.Native_string end

  module G = struct
    (* TODO(hammer): Use sosa *)
    (* TODO(hammer): Make terminator character explicit *)
    module Str = struct
      type t = string
      let default = ""
      let compare = Pervasives.compare
    end

    (* TODO(hammer): Use int option for vertex labels *)
    include Graph.Imperative.Digraph.AbstractLabeled(struct type t = unit end)(Str)
  end

  type t = { g:G.t; root:G.V.t; leaves:G.V.t array }

  let lcp s1 s2 =
    let split_hd = String.split_at ~index:1 in
    let rec loop s1 s2 n =
      match split_hd s1, split_hd s2 with
      | (hd1, tl1), (hd2, tl2) when hd1 = hd2 -> loop tl1 tl2 (n + 1)
      | _ -> n
    in
    loop s1 s2 0

  type split_info = { e:G.E.t; l_pre:int }
  let rec lcp_path st s es =
    let lcp_path_e st s e =
      let l = G.E.label e in
      let s_len = String.length s in
      let l_len = String.length l in
      let lcp_len = lcp s l in
      match () with
      | () when lcp_len = l_len && lcp_len < s_len ->
        let add_l_len x = (l_len + fst x, snd x) in
        let new_s = String.drop ~index:l_len s in
        let new_es = G.succ_e st.g (G.E.dst e) in
        add_l_len (lcp_path st new_s new_es)
      | () -> (lcp_len, Some {e; l_pre = lcp_len})
    in
    match es with
    | [] -> (-1, None)
    | _ -> BatList.reduce max (List.map (lcp_path_e st s) es)

  let put_suffix st i s =
    let leaf = G.V.create () in
    let es = G.succ_e st.g st.root in
    let s_len, split_info = lcp_path st s es in
    let e = G.E.create st.root s leaf in
    G.add_edge_e st.g e;
    st.leaves.(i) <- leaf

  let create s =
    let s_length = String.length s in
    let g = G.create () in
    let root = G.V.create () in
    G.add_vertex g root;
    let leaves = Array.make s_length (G.V.create ()) in
    let st = { g; root; leaves } in
    for i = 0 to s_length - 1 do
      let suffix = String.sub_exn s i (s_length - i) in
      put_suffix st i suffix;
    done;
    st

  let get_suffix st i =
    G.fold_pred_e (fun e s -> (G.E.label e) ^ s) st.g st.leaves.(i) ""
end



