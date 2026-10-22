(* Alpaca trading (v2) and market-data client. Every call returns a [result]; nothing raises on HTTP,
   network or decode errors. *)

type error =
  | Http of { status : int; body : string }
  | Network of string  (** connection failure or timeout, after retries *)
  | Decode of string

let pp_error fmt = function
  | Http { status; body } -> Format.fprintf fmt "HTTP %d: %s" status body
  | Network e -> Format.fprintf fmt "network error: %s" e
  | Decode e -> Format.fprintf fmt "decode error: %s" e

(** Retries apply to rate limits (429), server errors (5xx) and network failures. Order submission is only
    retried when it carries a [client_order_id], which Alpaca de-duplicates. *)
type retry = { attempts : int; base_delay : float; max_delay : float }

let default_retry = { attempts = 4; base_delay = 0.5; max_delay = 8. }
let no_retry = { default_retry with attempts = 1 }

type t = { env : Env.t; creds : Env.credentials; transport : Transport.t; sleep : float -> unit; retry : retry }

(** [sleep] defaults to [Unix.sleepf]; Eio programs should pass [Eio_transport.sleep clock]. *)
let create ?(sleep = Unix.sleepf) ?(retry = default_retry) ~env ~creds transport = { env; creds; transport; sleep; retry }

let retryable_status s = s = 429 || s >= 500

let call t ~meth ?body ?(query = []) ?(idempotent = true) ~base path =
  let uri = Uri.with_query' (Uri.with_path base path) query in
  let headers = Env.auth_headers t.creds in
  let rec attempt n =
    let result =
      match t.transport.request ~meth ~headers ?body uri with
      | r -> Ok r
      | exception e -> Error (Network (Printexc.to_string e))
    in
    let again () =
      t.sleep (Float.min t.retry.max_delay (t.retry.base_delay *. (2. ** float_of_int (n - 1))));
      attempt (n + 1)
    in
    let can_retry = n < t.retry.attempts in
    match result with
    | Ok { Transport.status; body } when status >= 200 && status < 300 -> Ok body
    (* a 429 means the request was rejected unprocessed, so it is always safe to retry *)
    | Ok { status = 429; _ } when can_retry -> again ()
    | Ok { status; _ } when retryable_status status && idempotent && can_retry -> again ()
    | Error (Network _) when idempotent && can_retry -> again ()
    | Ok { status; body } -> Error (Http { status; body })
    | Error e -> Error e
  in
  attempt 1

let decoded f = function
  | Ok body -> ( match Decode.parse f body with Ok v -> Ok v | Error e -> Error (Decode e))
  | Error e -> Error e

let trading t = Env.trading_url t.env
let get t ?query path f = decoded f (call t ~meth:`GET ?query ~base:(trading t) path)
let pct_encode s = Uri.pct_encode ~component:`Path s |> String.split_on_char '/' |> String.concat "%2F"

let account t = get t "/v2/account" Types.account
let positions t = get t "/v2/positions" Types.positions
let clock t = get t "/v2/clock" Types.clock
let open_orders t = get t ~query:[ ("status", "open"); ("limit", "500") ] "/v2/orders" Types.orders
let order t id = get t ("/v2/orders/" ^ id) Types.order
let order_by_client_id t coid = get t ~query:[ ("client_order_id", coid) ] "/v2/orders:by_client_order_id" Types.order

(** Tradability and fractionability, e.g. [asset t "SPY"] or [asset t "BTC/USD"]. *)
let asset t symbol = get t ("/v2/assets/" ^ pct_encode symbol) Types.asset

let is_duplicate_id = function
  | Http { status = 422; body } ->
      let re = "client_order_id" in
      let lb = String.length body and lr = String.length re in
      let rec has i = i + lr <= lb && (String.sub body i lr = re || has (i + 1)) in
      has 0
  | _ -> false

(** Submit an order. With a [client_order_id] the call is retried on transient failures, and if Alpaca
    reports the id as already used (e.g. a retry after a lost response, or re-running a job), the existing
    order is returned instead of an error. *)
let submit_order t (req : Types.order_request) =
  let body = Yojson.Safe.to_string (Types.order_request_to_json req) in
  let idempotent = Option.is_some req.req_client_order_id in
  match decoded Types.order (call t ~meth:`POST ~body ~idempotent ~base:(trading t) "/v2/orders") with
  | Error e when is_duplicate_id e -> (
      match req.req_client_order_id with
      | Some coid -> ( match order_by_client_id t coid with Ok o -> Ok o | Error _ -> Error e)
      | None -> Error e)
  | r -> r

let cancel_order t id = Result.map ignore (call t ~meth:`DELETE ~base:(trading t) ("/v2/orders/" ^ id))

(** Poll an order until it reaches a final status ([Types.is_final]) or [timeout] seconds pass. Returns the
    last state seen either way; check [status]. *)
let wait_for_order ?(timeout = 60.) ?(interval = 1.) t id =
  let deadline = Unix.gettimeofday () +. timeout in
  let rec loop () =
    match order t id with
    | Error e -> Error e
    | Ok o when Types.is_final o.status || Unix.gettimeofday () >= deadline -> Ok o
    | Ok _ ->
        t.sleep interval;
        loop ()
  in
  loop ()

let latest_prices t ~path symbols =
  if symbols = [] then Ok []
  else decoded Types.latest_trades (call t ~meth:`GET ~base:Env.data_url ~query:[ ("symbols", String.concat "," symbols) ] path)

let latest_stock_prices t symbols = latest_prices t ~path:"/v2/stocks/trades/latest" symbols
let latest_crypto_prices t symbols = latest_prices t ~path:"/v1beta3/crypto/us/latest/trades" symbols
