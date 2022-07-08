open Core
open Incr_dom
open! Js_of_ocaml
module Id = Unique_id.Int ()
module Node = Vdom.Node
module Attr = Vdom.Attr

module Visibility = struct
  type t =
    | All
    | Completed
    | Active
  [@@deriving sexp, compare]
end

module Todo = struct
  type t =
    { completed : bool
    ; text : string
    ; editing : bool
    }
  [@@deriving fields, sexp, compare]

  let of_text text = { completed = false; editing = false; text }
  let swap_completion x = { x with completed = not x.completed }

  let visible t ~(visibility : Visibility.t) =
    match visibility with
    | All -> true
    | Completed -> t.completed
    | Active -> not t.completed
  ;;

  let view
        ({ completed; text = todo_text; editing } as t)
        ~visibility
        ~change_completion
        ~destroy
        ~edit_started
        ~editing_ended
        ~set_text
    =
    let li_attrs =
      match
        List.concat
          [ (if completed then [ "completed" ] else [])
          ; (if not (visible t ~visibility) then [ "hidden" ] else [])
          ; (if editing then [ "editing" ] else [])
          ]
      with
      | [] -> []
      | classes -> [ Attr.classes classes ]
    in
    let input_attrs =
      [ Attr.class_ "toggle"; Attr.type_ "checkbox"; Attr.on_click change_completion ]
      @ if completed then [ Attr.checked ] else []
    in
    Node.li
      ~attr:(Attr.many_without_merge li_attrs)
      (List.concat
         [ [ Node.div
               ~attr:(Attr.class_ "view")
               (List.concat
                  [ (if not editing
                     then [ Node.input ~attr:(Attr.many_without_merge input_attrs) () ]
                     else [])
                  ; [ Node.label
                        ~attr:(Attr.on_double_click edit_started)
                        [ Node.text todo_text ]
                    ; Node.button
                        ~attr:
                          (Attr.many_without_merge
                             [ Attr.class_ "destroy"; Attr.on_click destroy ])
                        []
                    ]
                  ])
           ]
         ; (if not editing
            then []
            else
              [ Node.input
                  ~attr:
                    (Attr.many_without_merge
                       [ Attr.class_ "edit"
                       ; Attr.value todo_text
                       ; Attr.on_blur (fun _ -> editing_ended)
                       ; Vdom.Attr.on_change set_text
                       ])
                  ()
              ])
         ])
  ;;
end

module Model = struct
  type t =
    { todos : Todo.t Id.Map.t
    ; visibility : Visibility.t
    (* Determines if the main input element should be cleared of its' value *)
    ; clear_input : bool
    }
  [@@deriving fields, sexp, compare]

  let empty = Fields.create ~visibility:All ~todos:Id.Map.empty ~clear_input:true
  let cutoff t1 t2 = compare t1 t2 = 0
end

module Action = struct
  module Editing_state = struct
    type t =
      { editing : bool
      ; id : Id.t
      }
    [@@deriving sexp]
  end

  type t =
    | Clear_input of bool
    (* Clear the main input element of its' value? *)
    | Change_completion of Id.t
    | Set_text of Id.t * string
    | Delete of Id.t
    | Add of string
    | Clear_completed
    | Change_visibility of Visibility.t
    | Toggle_all
    | Editing_state of Editing_state.t
  [@@deriving sexp]
end

let apply_action ({ Model.todos; _ } as model) (action : Action.t) _ ~schedule_action:_
  : Model.t
  =
  let set_todos = Fieldslib.Field.fset Model.Fields.todos model in
  match action with
  | Clear_input clear_input -> { model with clear_input }
  | Change_completion id ->
    set_todos @@ Map.change todos id ~f:(Option.map ~f:Todo.swap_completion)
  | Delete id -> set_todos @@ Map.remove todos id
  | Add text -> set_todos @@ Map.set todos ~key:(Id.create ()) ~data:(Todo.of_text text)
  | Clear_completed -> set_todos @@ Map.filter todos ~f:(Fn.non Todo.completed)
  | Change_visibility x -> { model with visibility = x }
  | Toggle_all ->
    let set_completed_to = Map.exists todos ~f:(Fn.non Todo.completed) in
    set_todos @@ Map.map todos ~f:(fun t -> { t with completed = set_completed_to })
  | Set_text (id, text) ->
    set_todos
    @@ Map.change todos id ~f:(Option.map ~f:(fun (todo : Todo.t) -> { todo with text }))
  | Editing_state { editing; id } ->
    set_todos
    @@ Map.change
         todos
         id
         ~f:(Option.map ~f:(fun (todo : Todo.t) -> { todo with Todo.editing }))
;;

module State = struct
  type t = unit
end

let input_section inject clear_input =
  let input =
    let text_attr = if clear_input then [ Attr.string_property "value" "" ] else [] in
    Node.input
      ~attr:
        (Attr.many_without_merge
           ([ Attr.on_change (fun _ s ->
              Vdom.Effect.Many
                [ inject (Action.Clear_input true); inject (Action.Add s) ])
            ; Attr.class_ "new-todo"
            ; Attr.placeholder "What needs to be done?"
            ; Attr.autofocus true
            ]
            @ text_attr))
      ()
  in
  Node.header [ Node.h1 [ Node.text "todos" ]; input ]
;;

let info =
  Node.footer
    ~attr:(Attr.class_ "info")
    [ Node.p [ Node.text "Double-click to edit a todo" ]
    ; Node.p
        [ Node.text "part of "
        ; Node.a ~attr:(Attr.href "http://todomvc.com") [ Node.text "TodoMVC" ]
        ]
    ]
;;

let filters inject visibility =
  let buttons =
    List.map [ Visibility.All; Active; Completed ] ~f:(fun vis ->
      let name = String.capitalize @@ Sexp.to_string @@ Visibility.sexp_of_t vis in
      let attrs =
        let selected =
          if [%compare.equal: Visibility.t] vis visibility
          then [ Attr.class_ "selected" ]
          else []
        in
        Attr.href "#" :: selected
      in
      Node.li
        ~attr:(Attr.on_click (fun _ -> inject (Action.Change_visibility vis)))
        [ Node.a ~attr:(Attr.many_without_merge attrs) [ Node.text name ] ])
  in
  Node.ul ~attr:(Attr.class_ "filters") buttons
;;

let todo_count map_length =
  let pluralized_items =
    " item"
    ^
    match map_length with
    | 1 -> ""
    | _ -> "s"
  in
  Node.span
    ~attr:(Attr.class_ "todo-count")
    [ Node.strong [ Node.text (Int.to_string map_length) ]; Node.text pluralized_items ]
;;

let view model ~inject : Vdom.Node.t Incr.t =
  let open Incr.Let_syntax in
  let%map clear_input, footer, todos =
    let%bind visibility = model >>| Model.visibility in
    let%map view =
      (* Supposed to be hidden by default, shown when there are todos *)
      Incr.Map.filter_mapi (model >>| Model.todos) ~f:(fun ~key:id ~data:todo ->
        let editing_ended = inject (Action.Editing_state { editing = false; id }) in
        Option.return
        @@ Todo.view
             todo
             ~visibility
             ~change_completion:(fun _ -> inject (Action.Change_completion id))
             ~destroy:(fun _ -> inject (Action.Delete id))
             ~edit_started:(fun _ ->
               inject (Action.Editing_state { editing = true; id }))
             ~editing_ended
             ~set_text:(fun _elt str ->
               Vdom.Effect.Many [ inject (Action.Set_text (id, str)); editing_ended ]))
    and model = model
    and clear_input = model >>| Model.clear_input in
    let todos = Model.todos model in
    let node =
      if Map.is_empty todos
      then None (* Don't display anything *)
      else
        Some
          (let maybe_clear_completed =
             if Map.exists todos ~f:Todo.completed
             then
               [ Node.button
                   ~attr:
                     (Attr.many_without_merge
                        [ Attr.class_ "clear-completed"
                        ; Attr.on_click (fun _ -> inject Clear_completed)
                        ])
                   [ Node.text "Clear completed" ]
               ]
             else []
           in
           Node.footer
             ~attr:(Attr.class_ "footer")
             ([ todo_count (Map.length todos); filters inject visibility ]
              @ maybe_clear_completed))
    in
    clear_input, node, view
  in
  let main =
    Node.create
      "section"
      ~attr:(Attr.class_ "main")
      [ Node.input
          ~attr:
            (Attr.many_without_merge
               [ Attr.class_ "toggle-all"
               ; Attr.type_ "checkbox"
               ; Attr.on_click (fun _ -> inject Action.Toggle_all)
               ])
          ()
      ; Node.label ~attr:(Attr.for_ "toggle-all") [ Node.text "Mark all as complete" ]
      ; Node.ul ~attr:(Attr.class_ "todo-list") (Map.data todos)
      ]
  in
  Node.body
    [ Node.create
        "section"
        ~attr:(Attr.class_ "todoapp")
        (List.filter_opt [ Some (input_section inject clear_input); Some main; footer ])
    ; info
    ]
;;

let on_startup ~schedule_action:_ _ = Async_kernel.Deferred.unit

let on_display ~old_model _ ~schedule_action =
  (* If we are displaying when [clear_input] is [true], we can safely
     set it back to false, since the text entry will now be empty.
  *)
  if old_model.Model.clear_input then schedule_action (Action.Clear_input false);
  ()
;;

let create model ~old_model ~inject =
  let open Incr.Let_syntax in
  let%map apply_action =
    let%map model = model in
    apply_action model
  and on_display =
    let%map old_model = old_model in
    on_display ~old_model
  and view = view model ~inject
  and model = model in
  Component.create ~apply_action ~on_display model view
;;
