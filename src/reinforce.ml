open Tactician_ltac1_record_plugin
open Tactic_learner
open Dag_capnp_generator
open Dag_extractor

module Api = Graph_api.MakeRPC(Capnp_rpc_lwt)
open Capnp_rpc_lwt
open Lwt.Infix

module G = GlobalGraph

let service_name = Capnp_rpc_net.Restorer.Id.public ""

let rec execution_result s =
  let module ExecutionResult = Api.Builder.ExecutionResult in
  let module Graph = Api.Builder.Graph in
  let module ProofState = Api.Builder.ProofState in
  let new_state = ExecutionResult.new_state_init s in
  let graph = ExecutionResult.NewState.graph_init new_state in
  let _ = Graph.edges_set_list graph [] in
  let _ = Graph.classifications_set_list graph [] in
  let state = ExecutionResult.NewState.state_init new_state in
  ProofState.root_set_int_exn state 5;
  let _ = ProofState.context_set_list state [] in
  ExecutionResult.NewState.obj_set new_state (Some (obj ()))

and obj () =
  let module ProofObject = Api.Service.ProofObject in
  ProofObject.local @@ object
    inherit ProofObject.service

    method run_tactic_impl params release_param_caps =
      release_param_caps ();
      let open ProofObject.RunTactic in
      let response, results = Service.Response.create Results.init_pointer in
      let res = Results.result_init results in
      execution_result res;
      let tac = Api.Reader.Tactic.id_get (Params.tactic_get params) in
      print_endline "run tactic";
      print_endline (Stdint.Uint64.to_string tac);
      Service.return response
  end
let pull_reinforce =
  let module Reinforce = Api.Service.PullReinforce in
  Reinforce.local @@ object
    inherit Reinforce.service

    method reinforce_impl params release_param_caps =
      let open Reinforce.Reinforce in
      release_param_caps ();
      let response, results = Service.Response.create Results.init_pointer in
      let res = Results.result_init results in
      execution_result res;
      Service.return response
  end
let capnp_main conn =
  let module Main = Api.Service.Main in
  Main.local @@ object
    inherit Main.service

    method initialize_impl params release_param_caps =
      let open Main.Initialize in
      let response, results = Service.Response.create Results.init_pointer in
      let x = Params.push_get params in
      let callback s =
        let open Api.Client.PushReinforce.Reinforce in
        let request, params = Capability.Request.create Params.init_pointer in
        let res = Params.result_init params in
        execution_result res;
        Capability.call_for_unit_exn s method_id request in
      Service.return_lwt @@ fun () -> Capability.with_ref (Option.get x) @@ fun s ->
      callback s >>= fun () -> print_endline "called back";
      Results.pull_set results (Some pull_reinforce);
      release_param_caps ();
      Lwt.return @@ Ok response
  end


let () =
  Logs.set_level (Some Logs.Warning);
  Logs.set_reporter (Logs_fmt.reporter ())

let reinforce () =
  Lwt_main.run @@
  Lwt_switch.with_switch @@ fun switch ->
  let endpoint = Capnp_rpc_unix.Unix_flow.connect (Lwt_unix.of_unix_file_descr Unix.stdin)
                 |> Capnp_rpc_net.Endpoint.of_flow (module Capnp_rpc_unix.Unix_flow)
                   ~peer_id:Capnp_rpc_net.Auth.Digest.insecure
                   ~switch in
  let restore = Capnp_rpc_net.Restorer.single service_name (capnp_main endpoint) in
  let _ : Capnp_rpc_unix.CapTP.t = Capnp_rpc_unix.CapTP.connect ~restore endpoint in
  let w, f = Lwt.wait () in
  Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return @@ Lwt.wakeup f ());
  w
