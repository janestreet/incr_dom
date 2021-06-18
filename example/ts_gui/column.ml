open Core
open Import

module type T = sig
  type row

  module Contents : Stringable

  val name : string
  val group : string option
  val get : row -> Contents.t
  val set : row -> Contents.t -> row
  val editable : bool
  val focus_on_edit : bool
  val sort_by : Contents.t -> Sort_key.t
end

type 'a t = (module T with type row = 'a)

let create
      (type row contents)
      ~name
      ?group
      ?sort_by
      ?focus_on_edit
      (module Contents : Stringable with type t = contents)
      ~editable
      ~get
      ~set
  =
  let sort_by =
    match sort_by with
    | Some f -> f
    | None -> fun x -> Sort_key.String (Contents.to_string x)
  in
  (module struct
    type nonrec row = row

    module Contents = Contents

    let name = name
    let group = group
    let get = get
    let set = set
    let editable = editable
    let sort_by = sort_by

    let focus_on_edit =
      match focus_on_edit with
      | None -> false
      | Some () -> true
    ;;
  end : T
    with type row = row)
;;

let of_field
      (type contents)
      field
      ?group
      ?sort_by
      ?focus_on_edit
      (module Contents : Stringable with type t = contents)
      ~editable
  =
  create
    ~name:(Field.name field)
    ?group
    ?sort_by
    ?focus_on_edit
    (module Contents)
    ~editable
    ~get:(Field.get field)
    ~set:(Field.fset field)
;;

let name (type row) (module T : T with type row = row) = T.name
let group (type row) (module T : T with type row = row) = T.group
let editable (type row) (module T : T with type row = row) = T.editable
let focus_on_edit (type row) (module T : T with type row = row) = T.focus_on_edit

let get (type row) (module T : T with type row = row) row =
  T.Contents.to_string (T.get row)
;;

let set (type row) (module T : T with type row = row) row string =
  let open Or_error.Let_syntax in
  let%map v = Or_error.try_with (fun () -> T.Contents.of_string string) in
  T.set row v
;;

let sort_by (type row) (module T : T with type row = row) row = T.sort_by (T.get row)

let to_table_widget_column t =
  let name = name t in
  let group = group t in
  let sort_by _row_id row = sort_by t row in
  Ts_table.Column.create ~header:(Vdom.Node.text name) ~sort_by ?group ()
;;
