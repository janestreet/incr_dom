open Core
open Incr_dom
open Js_of_ocaml

module Model = struct
  type t = unit [@@deriving sexp]

  let init () = ()
  let cutoff _ _ = true
end

module Action = struct
  type t = unit [@@deriving sexp]

  let should_log _ = true
end

module State = struct
  type t = unit
end

let apply_action _ _ _ ~schedule_action:_ = ()
let on_startup ~schedule_action:_ _ = Async_kernel.return ()


let view_resize_observer =
  let widget_id =
    Type_equal.Id.create
      ~name:"my widget type"
      (fun (_ : ResizeObserver.resizeObserver Js.t * Dom_html.element Js.t) ->
         Sexp.Atom "<resize-observer-widget>")
  in
  Vdom.Node.widget
    ~id:widget_id
    ~init:(fun () ->
      let div = Dom_html.document##createElement (Js.string "div") in
      div##.className := Js.string "resize-div";
      let resize_observer =
        Js_of_ocaml.ResizeObserver.observe
          ~box:(Js.string "content-box")
          ~node:div
          ~f:(fun entries _observer ->
            Js_of_ocaml.Js.to_array entries
            |> Array.to_list
            |> List.iter ~f:(fun entry ->
              let width =
                entry##.contentRect##.width |> Js.Optdef.to_option |> Option.value_exn
              in
              let height =
                entry##.contentRect##.height |> Js.Optdef.to_option |> Option.value_exn
              in
              div##.textContent
              := Js.Opt.return (Js.string (sprintf "%f x %f" width height))))
          ()
      in
      resize_observer, div)
    ~destroy:(fun resize_observer _dom_node -> resize_observer##disconnect)
    ()
;;

let view_colorize_button =
  (* Each widget gets it's own Type_equal.Id.  You must re-use the same id whenever you
     want a widget from the previous render to be reused by a new render.  That's why we
     create the id just once, when [view] is called.  Be careful not to create type ids in
     the body of an incremental computation. *)
  let widget_id =
    Type_equal.Id.create ~name:"my widget type" (fun _ -> Sexp.Atom "<my widget type>")
  in
  (* Note that this is something of anti-pattern. We typically would only use widgets to
     include third-party JavaScript libraries. What we're doing here would be better done
     using [Incr_dom] in the ordinary way, but demonstrates the basic structure. *)
  Vdom.Node.widget
    ~id:widget_id
    ~init:(fun () ->
      (* in here we can do raw javascript operations! *)
      let button = Dom_html.document##createElement ("button" |> Js.string) in
      button##.innerHTML
      := "<b> I hearby agree to not use the widget API unless absolutely necessary </b>"
         |> Js.string;
      (* Our "colorify" widget is stored on the window object *)
      let colorify = Js.Unsafe.get Dom_html.window ("colorify" |> Js.string) in
      Js.Unsafe.fun_call colorify [| Js.Unsafe.inject button |] |> ignore;
      (), button)
    ()
;;

let raw_html =
  Vdom.Node.inner_html
    ()
    ~tag:"div"
    ~attrs:[ Vdom.Attr.empty ]
    ~this_html_is_sanitized_and_is_totally_safe_trust_me:"<b>bold</b>"
;;

let view _ ~inject:_ =
  Incr.return (Vdom.Node.div [ view_colorize_button; view_resize_observer; raw_html ])
;;

let create model ~old_model:_ ~inject =
  let open Incr.Let_syntax in
  let%map apply_action =
    let%map model = model in
    apply_action model
  and view = view model ~inject
  and model = model in
  Component.create ~apply_action model view
;;
