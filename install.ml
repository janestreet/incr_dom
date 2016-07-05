#use "topfind";;
#require "js-build-tools.oasis2opam_install";;

open Oasis2opam_install;;

generate ~package:"incr_dom"
  [ oasis_lib "incr_dom"
  ; file "META" ~section:"lib"
  ; file "_build/namespace_wrappers/js_of_ocaml_async.cmi" ~section:"lib"
  ; file "_build/namespace_wrappers/js_of_ocaml.cmi" ~section:"lib"
  ]
