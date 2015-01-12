module ST = struct
  (* TODO(hamer): Use sosa *)
  (* TODO(hamer): Make terminator character explicit *)
  module Str = struct
    type t = string
    let default = ""
    let compare = Pervasives.compare
  end

  (* TODO(hammer): Use int option for vertex labels *)
  include Graph.Imperative.Digraph.AbstractLabeled(struct type t = int end)(Str)
end



