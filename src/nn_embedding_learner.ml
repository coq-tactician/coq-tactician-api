open Tactician_ltac1_record_plugin
open Tactic_learner
open Dag_capnp_generator
open Dag_extractor

module NNEmbeddingLearner : TacticianOnlineLearnerType = functor (TS : TacticianStructures) -> struct
  module LH = Learner_helper.L(TS)
  open TS
  open LH

  module Api = Dag_api.MakeRPC(Capnp_rpc_lwt)
  module Embedding = Api.Client.Dag.Embedding
  open Capnp_rpc_lwt
  open Lwt.Infix

  module G = GlobalGraph
  module GB = DAGBuilder(G)
  open GB
  let gen_proof_state ps =
    let concl = proof_state_goal ps in
    let hyps = proof_state_hypotheses ps in
    with_named_context2 ~force:true (List.map (map_named term_repr) hyps) @@
    gen_constr @@ term_repr concl

  let embed t ps =
    let open Embedding.GenerateEmbedding in
    let request, params = Capability.Request.create Params.init_pointer in
    let params_db = Params.db_init params in
    let k = gen_proof_state ps in
    let (graph, root) = GB.run_empty false k in
    let nodes = List.map (fun (n, c) ->
        n, List.map (fun (_, i) -> 0, i) c) graph.dag.nodes in
    write_node_list params_db nodes;
    Params.ps_set_int_exn params @@ snd root;
    Capability.call_for_value_exn t method_id request >|= fun res ->
    let emb = Results.emb_get res in
    Capnp.Array.to_array emb

  let request_chan = Event.new_channel ()
  let response_chan = Event.new_channel ()
  let init_chan = Event.new_channel ()
  let () =
    let port =
      let open Unix in
      let sock = socket PF_INET SOCK_STREAM 0 in
      bind sock (ADDR_INET (inet_addr_any, 0));
      let port = match getsockname sock with
        | ADDR_UNIX _ -> assert false
        | ADDR_INET (_, p) -> p in
      close sock; port in
    ignore(Unix.create_process
             "python" [| "python"; "-m"; "nnserver"; string_of_int port |] Unix.stdin Unix.stdout Unix.stderr);
    let rec embedder_loop service =
      let e = Event.receive request_chan in
      let e = Event.sync e in
      embed service e >>= fun emb ->
      let e = Event.send response_chan emb in
      Event.sync e;
      embedder_loop service
    in
    let embedder () =
      Lwt_main.run @@
      let client_vat = Capnp_rpc_unix.client_only_vat () in
      let uri = Uri.make ~scheme:"capnp" ~userinfo:"insecure" ~host:"localhost" ~port:port () in
      let sr = Capnp_rpc_unix.Vat.import_exn client_vat @@ uri in
      let rec run_until_started () =
        Sturdy_ref.connect sr >>= function
        | Ok cap -> let e = Event.send init_chan () in Event.sync e; Lwt.return cap
        | Error {ty=`Failed; _} -> Lwt_unix.sleep 0.01 >>= run_until_started
        | Error e -> Lwt.fail_with (Fmt.to_to_string Capnp_rpc.Exception.pp e)
      in
      run_until_started () >>= fun cap ->
      Capability.with_ref cap embedder_loop in
    ignore(Thread.create embedder ());
    let e = Event.receive init_chan in
    Event.sync e

  let embedder ps =
      let e = Event.send request_chan ps in
      Event.sync e;
      let e = Event.receive response_chan in
      Event.sync e

  type model = (float array * tactic) list

  let not_normal_cosine v1 v2 =
    Array.fold_left (+.) 0. @@ Array.map2 (fun a b -> a *. b) v1 v2

  let empty () = []
  let learn db loc outcomes tac =
    List.map (fun outcome -> embedder outcome.before, tac) outcomes @ db
  let predict db f =
    if f = [] then IStream.empty else
      let emb = embedder (List.hd f).state in
      let out = List.map (fun (v, t) -> not_normal_cosine v emb, t) db in
      let out = remove_dups_and_sort out in
      let out = List.map (fun (a, c) -> { confidence = a; focus = 0; tactic = c }) out in
      IStream.of_list out
  let evaluate db _ _ = 0., db

end

(* let () = register_online_learner "NN kNN Learner" (module NNEmbeddingLearner) *)
