(** Typed client for the Alpaca trading and market-data REST APIs.

    {[
      Eio_main.run @@ fun env ->
          let creds = Result.get_ok (Camelid.Env.credentials_from_env ()) in
      let http = Camelid.Eio_transport.make ~clock:(Eio.Stdenv.clock env) (Eio.Stdenv.net env) in
      let c = Camelid.Client.create ~sleep:(Camelid.Eio_transport.sleep (Eio.Stdenv.clock env)) ~env:Paper ~creds http in
      match Camelid.Client.account c with
      | Ok a -> Printf.printf "equity %.2f\n" a.equity
      | Error e -> Format.printf "%a\n" Camelid.Client.pp_error e
    ]} *)

module Decode = Decode
module Env = Env
module Symbol = Symbol
module Types = Types
module Transport = Transport
module Eio_transport = Eio_transport
module Client = Client
