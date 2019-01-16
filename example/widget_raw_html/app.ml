open Core_kernel
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


let view _ ~inject:_ =
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
  Incr.return
    (Vdom.Node.widget
       ~id:widget_id
       ~init:(fun () ->
         (* in here we can do raw javascript operations! *)
         let button = Dom_html.document##createElement ("button" |> Js.string) in
         button##.innerHTML :=
           "<b> I hearby agree to not use the widget API unless absolutely necessary </b>"
           |> Js.string;
         (* Our "colorify" widget is stored on the window object *)
         let colorify = Js.Unsafe.get Dom_html.window ("colorify" |> Js.string) in
         Js.Unsafe.fun_call colorify [| Js.Unsafe.inject button |] |> ignore;
         (), button)
       ())
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
