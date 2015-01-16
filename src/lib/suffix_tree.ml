module ST = struct
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

  let create s =
    let s_length = String.length s in
    let g = G.create () in
    let root = G.V.create () in
    let leaves = Array.make s_length (G.V.create ()) in
    let st = { g; root; leaves } in
    for i = 0 to s_length - 1 do
      let leaf = G.V.create () in
      let label = String.sub s i (s_length - i) in
      let e = G.E.create st.root label leaf in
      G.add_edge_e st.g e;
      st.leaves.(i) <- leaf;
    done;
    st

  let get_suffix st i =
    G.fold_pred_e (fun e s -> (G.E.label e) ^ s) st.g st.leaves.(i) ""
end



