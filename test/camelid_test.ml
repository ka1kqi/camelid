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
    (fun (m, uri, key, _) -> Printf.printf "%s %s key=%s\n" (match m with `GET -> "GET" | `POST -> "POST" | `DELETE -> "DELETE") uri (Option.value ~default:"-" key))
    (List.rev !log);
  [%expect {|
    open=true
    HTTP 422: {"code":40010001,"message":"client_order_id must be unique"}
    HTTP 404: {"message":"not found"}
    GET https://paper-api.alpaca.markets/v2/clock key=KEY
    GET https://paper-api.alpaca.markets/v2/orders?status=open&limit=500 key=KEY
    GET https://data.alpaca.markets/v2/stocks/trades/latest?symbols=SPY%2CEFA key=KEY
    |}]
