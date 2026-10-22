(* Print paper-account equity, open positions and market status.

     export APCA_API_KEY_ID=... APCA_API_SECRET_KEY=...
     dune exec examples/account.exe *)

let () =
  Eio_main.run @@ fun env ->
  let creds = match Camelid.Env.credentials_from_env () with Ok c -> c | Error e -> failwith e in
  let client = Camelid.Client.create ~sleep:(Camelid.Eio_transport.sleep (Eio.Stdenv.clock env)) ~env:Paper ~creds
      (Camelid.Eio_transport.make ~clock:(Eio.Stdenv.clock env) (Eio.Stdenv.net env)) in
  let show = function Ok v -> v | Error e -> failwith (Format.asprintf "%a" Camelid.Client.pp_error e) in
  let account = show (Camelid.Client.account client) in
  let clock = show (Camelid.Client.clock client) in
  Printf.printf "equity $%.2f, cash $%.2f, market %s\n" account.equity account.cash (if clock.is_open then "open" else "closed");
  List.iter
    (fun (p : Camelid.Types.position) -> Printf.printf "  %-8s %12.6f @ %.2f\n" p.symbol p.qty p.current_price)
    (show (Camelid.Client.positions client))
