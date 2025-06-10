open! Core
open Js_of_ocaml

let print_errorf fmt = ksprintf (fun s -> Console.console##error (Js.string s)) fmt

module Filter = struct
  module String_blang = struct
    module T = struct
      type t = string Blang.t [@@deriving sexp, compare]
    end

    include T
    include Comparable.Make (T)
  end

  type t =
    | All
    | None
    | Named_filter_blang of String_blang.t
    | Custom_filter of (Sexp.t -> bool)
end

module Action_logging = struct
  type 'action t =
    { filter : Filter.t ref
    ; named_logging_filters : (string, 'action -> bool) Hashtbl.t
    }
end

module Flags = struct
  type t =
    { should_profile : unit -> bool
    ; should_debug : unit -> bool
    }
end

module App_state = struct
  type t =
    { filter_names : String.Set.t
    ; logging_filter : Filter.t ref
    ; should_profile : bool ref
    ; should_debug : bool ref
    }

  let set_logging_filter t ~logging_filter = t.logging_filter := logging_filter
  let set_should_profile t ~should_profile = t.should_profile := should_profile
  let set_should_debug t ~should_debug = t.should_debug := should_debug
end

class type global = object
  method startLoggingAll :
    (Js.js_string Js.t Js.opt -> unit) Js.callback Js.writeonly_prop

  method startLogging :
    (Js.js_string Js.t -> Js.js_string Js.t Js.opt -> unit) Js.callback Js.writeonly_prop

  method startLoggingCustom :
    ((Js.js_string Js.t -> bool Js.t) -> Js.js_string Js.t Js.opt -> unit) Js.callback
      Js.writeonly_prop

  method stopLogging : (Js.js_string Js.t Js.opt -> unit) Js.callback Js.writeonly_prop
  method startProfiling : (Js.js_string Js.t Js.opt -> unit) Js.callback Js.writeonly_prop
  method stopProfiling : (Js.js_string Js.t Js.opt -> unit) Js.callback Js.writeonly_prop
  method startDebugging : (Js.js_string Js.t Js.opt -> unit) Js.callback Js.writeonly_prop
  method stopDebugging : (Js.js_string Js.t Js.opt -> unit) Js.callback Js.writeonly_prop
  method saveIncrementalGraph : (unit -> unit) Js.callback Js.writeonly_prop
end

let global : global Js.t = Js.Unsafe.global
let global_is_initialized = ref false
let app_states : App_state.t String.Table.t = String.Table.create ()

let single_line_string_list strings =
  strings |> List.map ~f:(fun str -> "\"" ^ str ^ "\"") |> String.concat ~sep:", "
;;

let multi_line_string_list strings =
  strings |> List.map ~f:(fun str -> "  " ^ str) |> String.concat ~sep:"\n"
;;

let init_global ~app_filters () =
  let with_app_id_opt update_state app_id_opt =
    let app_id_opt = Js.Opt.to_option app_id_opt |> Option.map ~f:Js.to_string in
    match app_id_opt with
    | None -> Hashtbl.iter app_states ~f:update_state
    | Some app_id ->
      (match Hashtbl.find app_states app_id with
       | Some state -> update_state state
       | None ->
         print_errorf
           "Unable to find app with id \"%s\". Valid app ids are: %s"
           app_id
           (Hashtbl.keys app_states |> single_line_string_list))
  in
  let update_logging_filter logging_filter =
    with_app_id_opt (App_state.set_logging_filter ~logging_filter)
  in
  let update_should_profile should_profile =
    with_app_id_opt (App_state.set_should_profile ~should_profile)
  in
  let update_should_debug should_debug =
    with_app_id_opt (App_state.set_should_debug ~should_debug)
  in
  global##.startLoggingAll := Js.wrap_callback (update_logging_filter All);
  global##.startLogging
  := Js.wrap_callback (fun blang_str ->
       let blang_str = Js.to_string blang_str in
       with_app_id_opt (fun app_state ->
         let blang = Blang.t_of_sexp String.t_of_sexp (Sexp.of_string blang_str) in
         let invalid_names =
           Blang.fold blang ~init:String.Set.empty ~f:(fun invalid_names name ->
             if Set.mem app_state.filter_names name
             then invalid_names
             else Set.add invalid_names name)
         in
         if Set.is_empty invalid_names
         then
           App_state.set_logging_filter
             app_state
             ~logging_filter:(Named_filter_blang blang)
         else
           print_errorf
             "Unable to find named filter(s): %s. Valid names are:\n%s"
             (Set.to_list invalid_names |> single_line_string_list)
             (Set.to_list app_state.filter_names |> multi_line_string_list)));
  global##.startLoggingCustom
  := Js.wrap_callback (fun filter ->
       let filter action_sexp =
         action_sexp |> Sexp.to_string |> Js.string |> filter |> Js.to_bool
       in
       update_logging_filter (Custom_filter filter));
  global##.stopLogging := Js.wrap_callback (update_logging_filter None);
  global##.startProfiling := Js.wrap_callback (update_should_profile true);
  global##.stopProfiling := Js.wrap_callback (update_should_profile false);
  global##.startDebugging := Js.wrap_callback (update_should_debug true);
  global##.stopDebugging := Js.wrap_callback (update_should_debug false);
  global##.saveIncrementalGraph
  := Js.wrap_callback (fun () ->
       let filename = "current_incr_dom_dot_graph.dot" in
       Ui_incr.save_dot_to_file filename;
       let contents = In_channel.read_all filename in
       Vdom_file_download.create ~filename ~mimetype:"plain/text" ~contents
       |> Vdom_file_download.trigger);
  (* If we are running in jsdom, we don't want to log this starting message to the console. *)
  let group s ~f =
    if not am_running_test
    then (
      Console.console##groupCollapsed (Js.string s);
      f ();
      Console.console##groupEnd)
  in
  let log s = if not am_running_test then Console.console##log (Js.string s) in
  group "Incr_dom / Bonsai Console" ~f:(fun () ->
    group "Action Logging" ~f:(fun () ->
      log
        {|Logging prints action info to the console. It is disabled by default. To start logging, type one of the following:
startLoggingAll([app_id]) - log all actions
startLogging(filter_name [, app_id]) - filter actions using a pre-defined named filter [filter_name]
startLogging(filter_name_blang [, app_id]) - filter actions using a blang of named filters [filter_name_blang]
startLoggingCustom(filter [, app_id]) - filter actions using a custom function [filter] from a string (the action sexp) to a bool
To stop logging, type: stopLogging([app_id])|});
    group "Action Profiling" ~f:(fun () ->
      log
        {|Profiling is disabled by default.
To start profiling, type: startProfiling([app_id])
To stop profiling, type: stopProfiling([app_id])|});
    group "Debugging" ~f:(fun () ->
      log
        {|Debugging prints timing info to the console. It is disabled by default unless otherwise specified by the app.
To start debugging, type: startDebugging([app_id])
To stop debugging, type: stopDebugging([app_id])

[app_id] is equal to the id of the element that the incr-dom app is bound to. If the page only has one app or you want to apply the action to all apps, you can pass in [null] (or for single-argument functions, omit it altogether).|});
    log app_filters)
;;

let init_app ~app_id ~named_logging_filters ~initial_debug =
  let named_logging_filters =
    ("all", Fn.const true) :: ("none", Fn.const false) :: named_logging_filters
    |> String.Table.of_alist_exn
  in
  let filter_names = Hashtbl.keys named_logging_filters |> String.Set.of_list in
  let app_init_message =
    sprintf
      {|Available logging filters for "%s":
      %s|}
      app_id
      (Set.to_list filter_names |> multi_line_string_list)
  in
  if not !global_is_initialized
  then (
    init_global ~app_filters:app_init_message ();
    global_is_initialized := true)
  else if not am_running_test
  then Console.console##log (Js.string app_init_message);
  let logging_filter = ref Filter.None in
  let should_profile = ref false in
  let should_debug = ref initial_debug in
  Hashtbl.set
    app_states
    ~key:app_id
    ~data:{ filter_names; logging_filter; should_profile; should_debug };
  let action_logging =
    { Action_logging.filter = logging_filter; named_logging_filters }
  in
  let flags =
    { Flags.should_profile = (fun () -> !should_profile)
    ; should_debug = (fun () -> !should_debug)
    }
  in
  flags, action_logging
;;

let cleanup_app ~app_id = Hashtbl.remove app_states app_id

let maybe_log_action { Action_logging.filter; named_logging_filters } ~sexp_of_action =
  let safe_filter ~name filter action =
    match Or_error.try_with (fun () -> filter action) with
    | Ok should_log -> should_log
    | Error err ->
      print_errorf !"Exception raised by %s: %{Error#hum}" name err;
      false
  in
  let named_filter_blang_cache =
    Core.Memo.of_comparable (module Filter.String_blang) (fun blang ->
      let filter = Hashtbl.find_exn named_logging_filters in
      safe_filter
        ~name:(sprintf !"named filter blang \"%{sexp:string Blang.t}\"" blang)
        (match blang with
         | Base name -> filter name
         | _ -> fun action -> Blang.eval blang (fun name -> filter name action)))
  in
  fun action ->
    let should_log_action =
      match !filter with
      | All -> true
      | None -> false
      | Named_filter_blang blang -> named_filter_blang_cache blang action
      | Custom_filter filter ->
        safe_filter
          ~name:"custom filter"
          (fun action -> filter (sexp_of_action action))
          action
    in
    if should_log_action
    then (
      let action = sexp_of_action action in
      Async_js.log_s_as_string [%message "Action" (action : Sexp.t)])
;;
