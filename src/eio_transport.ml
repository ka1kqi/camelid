(* HTTPS transport on Eio + cohttp-eio + ocaml-tls (pure OCaml TLS, system CA bundle via ca-certs). *)

let tls_config () =
  match Ca_certs.authenticator () with
  | Error (`Msg m) -> failwith ("ca-certs: " ^ m)
  | Ok authenticator -> (
      match Tls.Config.client ~authenticator () with
      | Ok c -> c
      | Error (`Msg m) -> failwith ("tls config: " ^ m))

let https config uri raw =
  let host =
    Uri.host uri |> Option.map (fun h -> Domain_name.(host_exn (of_string_exn h)))
  in
  Tls_eio.client_of_flow ?host config raw

let max_body = 32 * 1024 * 1024

(* Each request (connect, send, read
   the whole body) must finish within [timeout] seconds or it raises [Eio.Time.Timeout]. *)
let make ?(timeout = 30.) ~clock (net : _ Eio.Net.t) : Transport.t =
  Mirage_crypto_rng_unix.use_default ();
  let client = Cohttp_eio.Client.make ~https:(Some (https (tls_config ()))) net in
  let request ~meth ~headers ?body uri =
    let headers = Cohttp.Header.of_list (("Accept", "application/json") :: headers) in
    let headers, body =
      match body with
      | Some b -> (Cohttp.Header.add headers "Content-Type" "application/json", Some (Cohttp_eio.Body.of_string b))
      | None -> (headers, None)
    in
    let meth = (meth :> Cohttp.Code.meth) in
    Eio.Time.with_timeout_exn clock timeout (fun () ->
        Eio.Switch.run @@ fun req_sw ->
        let resp, body = Cohttp_eio.Client.call client ~sw:req_sw ~headers ?body meth uri in
        let body = Eio.Buf_read.(parse_exn take_all) body ~max_size:max_body in
        { Transport.status = Cohttp.Code.code_of_status (Cohttp.Response.status resp); body })
  in
  { Transport.request }

(* A [sleep] for [Client.create] that yields to other fibers instead of blocking the domain. *)
let sleep clock seconds = Eio.Time.sleep clock seconds
