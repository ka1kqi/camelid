(* Endpoints and credentials. Uses the same environment variables as Alpaca's official SDKs. *)

type t = Paper | Live

type credentials = { key_id : string; secret_key : string }

let trading_url = function
  | Paper -> Uri.of_string "https://paper-api.alpaca.markets"
  | Live -> Uri.of_string "https://api.alpaca.markets"

let data_url = Uri.of_string "https://data.alpaca.markets"

let credentials_from_env () =
  match (Sys.getenv_opt "APCA_API_KEY_ID", Sys.getenv_opt "APCA_API_SECRET_KEY") with
  | Some key_id, Some secret_key when key_id <> "" && secret_key <> "" -> Ok { key_id; secret_key }
  | _ -> Error "set APCA_API_KEY_ID and APCA_API_SECRET_KEY"

let auth_headers c = [ ("APCA-API-KEY-ID", c.key_id); ("APCA-API-SECRET-KEY", c.secret_key) ]
