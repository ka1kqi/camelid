(* Typed views of the Alpaca v2 trading API objects we use. Unknown fields are ignored, so new API
   fields don't break decoding. *)

open Decode

type asset_class = Us_equity | Crypto | Other of string

let asset_class_of_string = function "us_equity" -> Us_equity | "crypto" -> Crypto | s -> Other s
let string_of_asset_class = function Us_equity -> "us_equity" | Crypto -> "crypto" | Other s -> s

type side = Buy | Sell

let string_of_side = function Buy -> "buy" | Sell -> "sell"
let side_of_string = function "buy" -> Buy | "sell" -> Sell | s -> fail "unknown side %S" s

type order_type = Market | Limit of float

type time_in_force = Day | Gtc | Opg | Cls | Ioc | Fok

let string_of_tif = function
  | Day -> "day" | Gtc -> "gtc" | Opg -> "opg" | Cls -> "cls" | Ioc -> "ioc" | Fok -> "fok"

type account = {
  account_id : string;
  status : string;
  currency : string;
  cash : float;
  equity : float;
  buying_power : float;
  trading_blocked : bool;
  account_blocked : bool;
  pattern_day_trader : bool;
}

let account json =
  {
    account_id = string "id" json;
    status = string "status" json;
    currency = string "currency" json;
    cash = number "cash" json;
    equity = number "equity" json;
    buying_power = number "buying_power" json;
    trading_blocked = bool "trading_blocked" json;
    account_blocked = bool "account_blocked" json;
    pattern_day_trader = bool "pattern_day_trader" json;
  }

type position = {
  symbol : string;  (** canonical: "BTC/USD", not "BTCUSD" *)
  asset_class : asset_class;
  qty : float;  (** negative for shorts *)
  market_value : float;
  current_price : float;
  avg_entry_price : float;
}

let position json =
  let ac = string "asset_class" json in
  let qty = number "qty" json in
  let qty = if string_opt "side" json = Some "short" && qty > 0. then -.qty else qty in
  {
    symbol = Symbol.canonical ~asset_class:ac (string "symbol" json);
    asset_class = asset_class_of_string ac;
    qty;
    market_value = number "market_value" json;
    current_price = number "current_price" json;
    avg_entry_price = number "avg_entry_price" json;
  }

let positions json = list position json

type clock = { timestamp : string; is_open : bool; next_open : string; next_close : string }

let clock json =
  {
    timestamp = string "timestamp" json;
    is_open = bool "is_open" json;
    next_open = string "next_open" json;
    next_close = string "next_close" json;
  }

type order = {
  id : string;
  client_order_id : string;
  symbol : string;
  status : string;
  side : side;
  qty : float option;
  notional : float option;
  filled_qty : float;
  filled_avg_price : float option;
  time_in_force : string;
  submitted_at : string option;
}

let order json =
  {
    id = string "id" json;
    client_order_id = string "client_order_id" json;
    symbol = string "symbol" json;
    status = string "status" json;
    side = side_of_string (string "side" json);
    qty = number_opt "qty" json;
    notional = number_opt "notional" json;
    filled_qty = Option.value ~default:0. (number_opt "filled_qty" json);
    filled_avg_price = number_opt "filled_avg_price" json;
    time_in_force = string "time_in_force" json;
    submitted_at = string_opt "submitted_at" json;
  }

let orders json = list order json

type amount = Qty of float | Notional of float

type order_request = {
  req_symbol : string;
  req_side : side;
  amount : amount;
  req_type : order_type;
  tif : time_in_force;
  req_client_order_id : string option;
}

(* Alpaca expects decimal strings; 9 decimals covers crypto lot sizes. *)
let decimal f =
  let s = Printf.sprintf "%.9f" f in
  let s = if String.contains s '.' then
      let i = ref (String.length s - 1) in
      while s.[!i] = '0' do decr i done;
      if s.[!i] = '.' then String.sub s 0 !i else String.sub s 0 (!i + 1)
    else s in
  s

let order_request_to_json r : Yojson.Safe.t =
  let amount = match r.amount with Qty q -> ("qty", `String (decimal q)) | Notional n -> ("notional", `String (decimal n)) in
  let typ = match r.req_type with Market -> [ ("type", `String "market") ] | Limit p -> [ ("type", `String "limit"); ("limit_price", `String (decimal p)) ] in
  let coid = match r.req_client_order_id with Some c -> [ ("client_order_id", `String c) ] | None -> [] in
  `Assoc
    ([ ("symbol", `String r.req_symbol); amount; ("side", `String (string_of_side r.req_side)); ("time_in_force", `String (string_of_tif r.tif)) ]
    @ typ @ coid)

(* Market data: latest trade price per symbol, from {"trades": {"SPY": {"p": 581.2, ...}, ...}}. *)
let latest_trades json =
  match member "trades" json with
  | `Assoc kv -> List.map (fun (sym, t) -> (sym, number "p" t)) kv
  | `Null -> []
  | _ -> fail "trades: expected an object"

(* Order statuses after which nothing more will happen. *)
let is_final = function
  | "filled" | "canceled" | "expired" | "rejected" | "replaced" | "done_for_day" -> true
  | _ -> false

type asset = {
  a_symbol : string;
  a_class : asset_class;
  a_status : string;  (** "active" or "inactive" *)
  tradable : bool;
  fractionable : bool;
  shortable : bool;
}

let asset json =
  let ac = string "class" json in
  {
    a_symbol = Symbol.canonical ~asset_class:ac (string "symbol" json);
    a_class = asset_class_of_string ac;
    a_status = string "status" json;
    tradable = bool "tradable" json;
    fractionable = bool "fractionable" json;
    shortable = bool "shortable" json;
  }
