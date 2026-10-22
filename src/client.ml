(* Alpaca trading (v2) and market-data client. Every call returns a [result]; nothing raises on HTTP or
   decode errors. *)

type error = Http of { status : int; body : string } | Decode of string

let pp_error fmt = function
  | Http { status; body } -> Format.fprintf fmt "HTTP %d: %s" status body
  | Decode e -> Format.fprintf fmt "decode error: %s" e

type t = { env : Env.t; creds : Env.credentials; transport : Transport.t }

let create ~env ~creds transport = { env; creds; transport }

let call t ~meth ?body ?(query = []) ~base path decoder =
  let uri = Uri.with_query' (Uri.with_path base path) query in
  let headers = Env.auth_headers t.creds in
  let { Transport.status; body } = t.transport.request ~meth ~headers ?body uri in
  if status >= 200 && status < 300 then
    match decoder with
    | None -> Ok `Null
    | Some f -> ( match Decode.parse f body with Ok v -> Ok (`Value v) | Error e -> Error (Decode e))
  else Error (Http { status; body })

let get t ?query path f =
  match call t ~meth:`GET ?query ~base:(Env.trading_url t.env) path (Some f) with
  | Ok (`Value v) -> Ok v
  | Ok `Null -> Error (Decode "empty response")
  | Error e -> Error e

let account t = get t "/v2/account" Types.account
let positions t = get t "/v2/positions" Types.positions
let clock t = get t "/v2/clock" Types.clock
let open_orders t = get t ~query:[ ("status", "open"); ("limit", "500") ] "/v2/orders" Types.orders

let submit_order t (req : Types.order_request) =
  let body = Yojson.Safe.to_string (Types.order_request_to_json req) in
  match call t ~meth:`POST ~body ~base:(Env.trading_url t.env) "/v2/orders" (Some Types.order) with
  | Ok (`Value o) -> Ok o
  | Ok `Null -> Error (Decode "empty response")
  | Error e -> Error e

let cancel_order t id =
  match call t ~meth:`DELETE ~base:(Env.trading_url t.env) ("/v2/orders/" ^ id) None with
  | Ok _ -> Ok ()
  | Error e -> Error e

let latest_prices t ~path symbols =
  if symbols = [] then Ok []
  else
    match
      call t ~meth:`GET ~base:Env.data_url ~query:[ ("symbols", String.concat "," symbols) ] path
        (Some Types.latest_trades)
    with
    | Ok (`Value v) -> Ok v
    | Ok `Null -> Ok []
    | Error e -> Error e

let latest_stock_prices t symbols = latest_prices t ~path:"/v2/stocks/trades/latest" symbols
let latest_crypto_prices t symbols = latest_prices t ~path:"/v1beta3/crypto/us/latest/trades" symbols
