include Incr_dom
include Incr.Let_syntax
module Js = Js_of_ocaml.Js
module Dom_html = Js_of_ocaml.Dom_html
include Splay_tree.Std
include Incr_dom_partial_render

module Row_id = struct
  include Core.Unique_id.Int ()
end

module Ts_table = Table.Make (Row_id) (Core.Int) (Table.Default_sort_spec)
module Sort_key = Table.Default_sort_spec.Sort_key
