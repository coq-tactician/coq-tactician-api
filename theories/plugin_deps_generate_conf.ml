(* The purpose of this module is load recursively the transitive
   dynamical ocaml plugin dependencies (Dynload *.cmxs objects) before
   upstream Coq implements properly recursive loading of ocaml plugin
   dependencies.

 In this way we get rid of manual maintance of recursive list of
   dynamical deps in NNLearner.v *)

(* Packages on which we depends.  Must keep in sync with dune. TODO:
   write preprocessor to extract from dune *)

let pkgs =  [ "logs.fmt";
              "capnp";
              "capnp.unix";
              "capnp-rpc-unix";
              "capnp-rpc-lwt";
              "lwt.unix";
              "ocamlgraph";]


let hard_linked = ["unix"]  (* list of hardlinked libraries to coq *)

let bad_meta_pkg_list = ["zarith"]   (* list of findlib pkg names that
                                        have bugs:
                                        https://github.com/ocaml/Zarith/issues/102
                                        *)

let bad_meta_sub_map = [ ("zarith.cmxa", "zarith") ] (* replacement map for bad cases *)

let preds = ["native";
             "ppx_driver";
             "mt";
             "mt_posix"]

let debug_msg = Printf.eprintf "%s\n" (* TODO: replace by standard log/debug instruments *)


let strip_cmxs s =
  let open String in
  let n = length s in if n >=5 && sub s (n - 5) 5  = ".cmxs" then  sub s 0 (n - 5)
                      else raise (Invalid_argument ("Error: library filename " ^ s ^ " does not end in *.cmxs"))


let strip_ext s =
  match List.find_opt (fun (k,v)  -> k = s) bad_meta_sub_map with
  | Some (k,v) -> v
  | None ->  strip_cmxs s

let quote s = "\"" ^ s ^ "\""

let write_loader_line dir filename =
  "Add ML Path " ^ (quote dir) ^ ". Declare ML Module " ^ (quote filename) ^ ".\n"

let write_injection_flag_line dir =
  "-I " ^ dir ^ " "


let print_two_streams out1 out2 dir filename = (
    Printf.fprintf out1 "%s" (write_loader_line dir (strip_ext filename));
    Printf.fprintf out2 "%s" (write_injection_flag_line dir);
  )

let in_words s = Str.split (Str.regexp "[ \t\n\r,]+") s (* https://github.com/ocaml/ocamlfind/src/findlib/fl_split.ml#L7 *)

let load_pkg printer pkg =
  if not (Findlib.is_recorded_package pkg) &&
       not (List.mem pkg hard_linked) then (
     let dir = Findlib.package_directory pkg in
     let preds = Findlib.recorded_predicates() in
     let archive =
       try
         Findlib.package_property preds pkg "plugin"
       with
         | Not_found ->
              try
                let v, fpreds =
                  Findlib.package_property_2 ("plugin"::preds) pkg "archive" in
                let need_plugin =
                  List.mem "native" preds in
                if need_plugin
                   && not (List.mem (`Pred "plugin") fpreds)
                   && not (List.mem pkg bad_meta_pkg_list) then   (debug_msg @@ "Skipping: " ^ v; "")
                else v
              with Not_found -> "" in
     let files = in_words archive in
     List.iter (printer dir) files;
     Findlib.record_package Findlib.Record_load pkg
  )


let () =
  if (Array.length Sys.argv) != 3 then (
    debug_msg "usage: ocaml plugin_deps_generate_conf.ml \
               TacticianReinforceDepLoader.v injection-flags";
    exit 1)
  else (
    debug_msg "Using findlib search path:";
    Findlib.init ();
    List.iter (debug_msg) (Findlib.search_path ());
    let resolved_pkgs = Findlib.package_deep_ancestors preds pkgs in
    Findlib.record_package_predicates preds;
    let out1 = open_out Sys.argv.(1) in
    let out2 = open_out Sys.argv.(2) in
    List.iter (load_pkg (print_two_streams out1 out2)) resolved_pkgs;
    close_out out1;
    close_out out2;
  )
