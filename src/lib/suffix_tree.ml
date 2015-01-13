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

  type t = { g : G.t; leaves : G.V.t array }

  let create s =
    let g = G.create () in
    let v1 = G.V.create () in
    let v2 = G.V.create () in
    let e = G.E.create v1 s v2 in
    G.add_edge_e g e;
    let leaves = [| v2 |]  in
    { g; leaves }

  let get_suffix st i =
    G.fold_pred_e (fun e s -> (G.E.label e) ^ s) st.g st.leaves.(i) ""
end



