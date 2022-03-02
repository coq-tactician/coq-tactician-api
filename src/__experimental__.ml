(* EXPERIMENTAL CODE *)

let handle_connection ?tags ~switch ~secret_key vat client =
  Lwt.catch (fun () ->
      let raw_flow = Capnp_rpc_unix.Unix_flow.connect ~switch client in
      Capnp_rpc_unix.Network.accept_connection ~switch ~secret_key raw_flow >>= function
      | Error (`Msg msg) ->
        Logs.warn (fun f -> f ?tags "Rejecting new connection: %s" msg);
        Lwt.return_unit
      | Ok ep ->
        Capnp_rpc_unix.Vat.add_connection vat ~switch ~mode:`Accept ep >|= fun (_ : Capnp_rpc_unix.CapTP.t) ->
        ()
    )
    (fun ex ->
       Logs.err (fun f -> f "Uncaught exception handling connection: %a" Fmt.exn ex);
       Lwt.return_unit
    )

let reinforce_file_descr_new connected_socket =
  Lwt_main.run (
      let w, finish = Lwt.wait () in
      let switch = Lwt_switch.create () in
      let () = Lwt_switch.add_hook (Some switch) (fun () -> Lwt.return (Lwt.wakeup finish ())) in
      let restore = Capnp_rpc_net.Restorer.single (Capnp_rpc_net.Restorer.Id.public "") capnp_main in
      let a = Unix.getsockname connected_socket in
      let b =
        match a with
        | Unix.ADDR_INET (ahost, aport) -> Capnp_rpc_unix.Network.Location.tcp ~host:(Unix.string_of_inet_addr(ahost)) ~port:aport
        | Unix.ADDR_UNIX afile -> Capnp_rpc_unix.Network.Location.unix afile
      in
      let vat = Capnp_rpc_unix.Vat.create ~restore ~address:(b,Capnp_rpc_net.Auth.Digest.insecure)
              ~secret_key:(Lazy.from_val (Capnp_rpc_net.Auth.Secret_key.generate ())) () in
      let () = Capnp_rpc_unix.Vat.dump (Format.formatter_of_out_channel stderr) vat in
      let%lwt _ = handle_connection ~switch:switch ~secret_key:None vat (Lwt_unix.of_unix_file_descr connected_socket) in
      w
    )


let get_message () =
  let promise = Lwt_io.read_line Lwt_io.stdin in
  let msg = Lwt_main.run promise in
  Bytes.of_string msg

let connect ip_addr port =
  Printf.eprintf "connecting to %s:%d\n" ip_addr port;
  let my_socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  let server_addr = Unix.ADDR_INET (Unix.inet_addr_of_string ip_addr, port) in
  let message = get_message () in
  Unix.connect my_socket server_addr;
  Printf.eprintf "sent %d bytes" (Unix.write my_socket message 0 (Bytes.length message))
  (* Unix.shutdown my_socket Unix.SHUTDOWN_ALL *)
