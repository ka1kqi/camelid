open Camelid

let decode f s = match Decode.parse f s with Ok v -> v | Error e -> failwith e

let%expect_test "account: numbers arrive as strings" =
  let a =
    decode Types.account
      {|{"id":"abc","status":"ACTIVE","currency":"USD","cash":"1234.5","equity":"10000.25",
         "buying_power":"20000.50","trading_blocked":false,"account_blocked":false,
         "pattern_day_trader":false,"some_new_field":42}|}
  in
  Printf.printf "%s %s equity=%.2f cash=%.2f bp=%.2f blocked=%b\n" a.account_id a.status a.equity a.cash a.buying_power a.trading_blocked;
  [%expect {| abc ACTIVE equity=10000.25 cash=1234.50 bp=20000.50 blocked=false |}]

let%expect_test "positions: crypto symbols are canonicalised, shorts are negative" =
  let ps =
    decode Types.positions
      {|[{"symbol":"SPY","asset_class":"us_equity","qty":"3.5","side":"long","market_value":"2100",
          "current_price":"600","avg_entry_price":"590"},
         {"symbol":"BTCUSD","asset_class":"crypto","qty":"0.01","side":"long","market_value":"800",
          "current_price":"80000","avg_entry_price":"70000"},
         {"symbol":"TLT","asset_class":"us_equity","qty":"2","side":"short","market_value":"-180",
          "current_price":"90","avg_entry_price":"95"}]|}
  in
  List.iter (fun (p : Types.position) -> Printf.printf "%s %s %g\n" p.symbol (Types.string_of_asset_class p.asset_class) p.qty) ps;
  [%expect {|
    SPY us_equity 3.5
    BTC/USD crypto 0.01
    TLT us_equity -2
    |}]

let%expect_test "symbol canonicalisation" =
  List.iter
    (fun (ac, s) -> print_endline (Symbol.canonical ~asset_class:ac s))
    [ ("crypto", "BTCUSD"); ("crypto", "ETH/USD"); ("crypto", "SOLUSDT"); ("us_equity", "BTCUSD") ];
  [%expect {|
    BTC/USD
    ETH/USD
    SOL/USDT
    BTCUSD
    |}]

let%expect_test "order request JSON" =
  let req =
    { Types.req_symbol = "BTC/USD"; req_side = Buy; amount = Qty 0.012500; req_type = Market; tif = Gtc; req_client_order_id = Some "order-1" }
  in
  print_endline (Yojson.Safe.to_string (Types.order_request_to_json req));
  let req = { Types.req_symbol = "SPY"; req_side = Sell; amount = Qty 3.; req_type = Limit 581.25; tif = Cls; req_client_order_id = None } in
  print_endline (Yojson.Safe.to_string (Types.order_request_to_json req));
  [%expect {|
    {"symbol":"BTC/USD","qty":"0.0125","side":"buy","time_in_force":"gtc","type":"market","client_order_id":"order-1"}
    {"symbol":"SPY","qty":"3","side":"sell","time_in_force":"cls","type":"limit","limit_price":"581.25"}
    |}]

let%expect_test "latest trades" =
  let ts = decode Types.latest_trades {|{"trades":{"SPY":{"t":"2026-10-22T19:35:00Z","p":602.5,"s":100},"EFA":{"p":"88.1"}}}|} in
  List.iter (fun (s, p) -> Printf.printf "%s %.2f\n" s p) ts;
  [%expect {|
    SPY 602.50
    EFA 88.10
    |}]

(* A fake transport records requests and replays canned responses. *)
let mock responses =
  let log = ref [] in
  let request ~meth ~headers ?body uri =
    log := (meth, Uri.to_string uri, List.assoc_opt "APCA-API-KEY-ID" headers, body) :: !log;
    let path = Uri.path uri in
    match List.assoc_opt path responses with
    | Some (status, body) -> { Transport.status; body }
    | None -> { Transport.status = 404; body = {|{"message":"not found"}|} }
  in
  ({ Transport.request }, log)

let creds = { Env.key_id = "KEY"; secret_key = "SECRET" }

let%expect_test "client: paper URL, auth header, query string, errors" =
  let transport, log =
    mock
      [ ("/v2/clock", (200, {|{"timestamp":"t","is_open":true,"next_open":"o","next_close":"c"}|}));
        ("/v2/orders", (422, {|{"code":40010001,"message":"client_order_id must be unique"}|})) ]
  in
  let c = Client.create ~env:Paper ~creds transport in
  (match Client.clock c with Ok k -> Printf.printf "open=%b\n" k.is_open | Error _ -> print_endline "clock failed");
  (match Client.open_orders c with Ok _ -> () | Error e -> Format.printf "%a@." Client.pp_error e);
  (match Client.latest_stock_prices c [ "SPY"; "EFA" ] with Ok _ -> () | Error e -> Format.printf "%a@." Client.pp_error e);
  List.iter
    (fun (m, uri, key, _) -> Printf.printf "%s %s key=%s\n" (match m with `GET -> "GET" | `POST -> "POST" | `PATCH -> "PATCH" | `DELETE -> "DELETE") uri (Option.value ~default:"-" key))
    (List.rev !log);
  [%expect {|
    open=true
    HTTP 422: {"code":40010001,"message":"client_order_id must be unique"}
    HTTP 404: {"message":"not found"}
    GET https://paper-api.alpaca.markets/v2/clock key=KEY
    GET https://paper-api.alpaca.markets/v2/orders?status=open&limit=500 key=KEY
    GET https://data.alpaca.markets/v2/stocks/trades/latest?symbols=SPY%2CEFA key=KEY
    |}]

(* A transport that replays a script of responses per path, and counts calls. *)
let scripted script =
  let calls = Hashtbl.create 8 in
  let request ~meth ~headers:_ ?body:_ uri =
    let key = ((match meth with `GET -> "GET" | `POST -> "POST" | `PATCH -> "PATCH" | `DELETE -> "DELETE"), Uri.path uri) in
    let n = Option.value ~default:0 (Hashtbl.find_opt calls key) in
    Hashtbl.replace calls key (n + 1);
    match List.assoc_opt key script with
    | Some steps -> (
        (* past the end of the script, keep repeating the last step *)
        let step = match List.nth_opt steps n with Some s -> s | None -> List.hd (List.rev steps) in
        match step with `Raise msg -> failwith msg | `Resp (status, body) -> { Transport.status; body })
    | None -> { Transport.status = 404; body = "{}" }
  in
  ({ Transport.request }, fun key -> Option.value ~default:0 (Hashtbl.find_opt calls key))

let sleeps = ref []
let client_of transport = Client.create ~sleep:(fun s -> sleeps := s :: !sleeps) ~env:Paper ~creds transport

let order_json status = Printf.sprintf
  {|{"id":"o1","client_order_id":"c1","symbol":"SPY","status":"%s","side":"buy","qty":"2","filled_qty":"%s",
     "filled_avg_price":%s,"time_in_force":"day"}|} status (if status = "filled" then "2" else "0") (if status = "filled" then {|"600.5"|} else "null")

let clock_ok = `Resp (200, {|{"timestamp":"t","is_open":true,"next_open":"o","next_close":"c"}|})

let%expect_test "GET retries 429, 503 and network errors with backoff, then succeeds" =
  sleeps := [];
  let tr, count = scripted [ (("GET", "/v2/clock"), [ `Resp (429, "slow down"); `Resp (503, "busy"); `Raise "connection reset"; clock_ok ]) ] in
  (match Client.clock (client_of tr) with Ok c -> Printf.printf "open=%b" c.is_open | Error e -> Format.printf "%a@?" Client.pp_error e);
  Printf.printf " calls=%d sleeps=%s\n" (count ("GET", "/v2/clock")) (String.concat "," (List.rev_map string_of_float !sleeps));
  [%expect {| open=true calls=4 sleeps=0.5,1.,2. |}]

let%expect_test "gives up after the attempt limit; network errors are values, not exceptions" =
  let tr, count = scripted [ (("GET", "/v2/clock"), [ `Raise "timeout" ]) ] in
  (match Client.clock (client_of tr) with Ok _ -> () | Error e -> Format.printf "%a@?" Client.pp_error e);
  Printf.printf " calls=%d\n" (count ("GET", "/v2/clock"));
  [%expect {| network error: Failure("timeout") calls=4 |}]

let req coid =
  { Types.req_symbol = "SPY"; req_side = Buy; amount = Qty 2.; req_type = Market; tif = Day; req_client_order_id = coid }

let%expect_test "orders without client_order_id are never retried on 5xx" =
  let tr, count = scripted [ (("POST", "/v2/orders"), [ `Resp (502, "bad gateway"); `Resp (200, order_json "new") ]) ] in
  (match Client.submit_order (client_of tr) (req None) with Ok _ -> print_string "ok" | Error e -> Format.printf "%a@?" Client.pp_error e);
  Printf.printf " calls=%d\n" (count ("POST", "/v2/orders"));
  [%expect {| HTTP 502: bad gateway calls=1 |}]

let%expect_test "with client_order_id: retried, and a duplicate-id rejection returns the existing order" =
  let tr, count =
    scripted
      [ (("POST", "/v2/orders"), [ `Raise "timeout"; `Resp (422, {|{"code":40010001,"message":"client_order_id must be unique"}|}) ]);
        (("GET", "/v2/orders:by_client_order_id"), [ `Resp (200, order_json "accepted") ]) ]
  in
  (match Client.submit_order (client_of tr) (req (Some "c1")) with
  | Ok o -> Printf.printf "%s %s" o.id o.status
  | Error e -> Format.printf "%a@?" Client.pp_error e);
  Printf.printf " posts=%d lookups=%d\n" (count ("POST", "/v2/orders")) (count ("GET", "/v2/orders:by_client_order_id"));
  [%expect {| o1 accepted posts=2 lookups=1 |}]

let%expect_test "wait_for_order polls until a final status" =
  sleeps := [];
  let tr, count = scripted [ (("GET", "/v2/orders/o1"), [ `Resp (200, order_json "new"); `Resp (200, order_json "partially_filled"); `Resp (200, order_json "filled") ]) ] in
  (match Client.wait_for_order ~interval:0.25 (client_of tr) "o1" with
  | Ok o -> Printf.printf "%s filled=%g @ %s" o.status o.filled_qty (Option.fold ~none:"-" ~some:string_of_float o.filled_avg_price)
  | Error e -> Format.printf "%a@?" Client.pp_error e);
  Printf.printf " polls=%d sleeps=%d\n" (count ("GET", "/v2/orders/o1")) (List.length !sleeps);
  [%expect {| filled filled=2 @ 600.5 polls=3 sleeps=2 |}]

let%expect_test "assets: crypto symbol is path-encoded and canonicalised" =
  let seen = ref "" in
  let transport =
    { Transport.request = fun ~meth:_ ~headers:_ ?body:_ uri ->
        seen := Uri.to_string uri;
        { status = 200; body = {|{"symbol":"BTC/USD","class":"crypto","status":"active","tradable":true,"fractionable":true,"shortable":false}|} } }
  in
  (match Client.asset (client_of transport) "BTC/USD" with
  | Ok a -> Printf.printf "%s tradable=%b fractionable=%b\n%s\n" a.a_symbol a.tradable a.fractionable !seen
  | Error e -> Format.printf "%a@?" Client.pp_error e);
  [%expect {|
    BTC/USD tradable=true fractionable=true
    https://paper-api.alpaca.markets/v2/assets/BTC%2FUSD
    |}]
