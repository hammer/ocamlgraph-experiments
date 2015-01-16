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

  type t = { g : G.t; root : G.V.t; leaves : G.V.t array }

  let lcp s1 s2 =
    let split_hd = String.split_at ~index:1 in
    let rec loop s1 s2 n =
      match split_hd s1, split_hd s2 with
      | (hd1, tl1), (hd2, tl2) when hd1 = hd2 -> loop tl1 tl2 (n + 1)
      | _ -> n
    in
    loop s1 s2 0

  let put_suffix st i s =
    let leaf = G.V.create () in
    let e = G.E.create st.root s leaf in
    G.add_edge_e st.g e;
    st.leaves.(i) <- leaf

  let create s =
    let s_length = String.length s in
    let g = G.create () in
    let root = G.V.create () in
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



