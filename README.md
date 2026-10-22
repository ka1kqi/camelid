# camelid

A typed OCaml client for the [Alpaca](https://alpaca.markets) trading and market-data REST APIs.

Alpacas and camels are both camelids. So is this library: Alpaca, in OCaml.

- **Trading API (v2):** account, positions, orders (submit, list open, cancel), market clock. Paper and live.
- **Market data:** latest trade prices for US stocks and crypto.
- **Typed and tolerant:** Alpaca sends most numbers as strings; camelid decodes them to `float`, ignores
  unknown fields, and normalises crypto symbols (`BTCUSD` in positions, `BTC/USD` in orders) to one form.
- **No exceptions for API errors:** every call returns `(_, Client.error) result`, including network failures
  and timeouts.
- **Retries:** rate limits (429), 5xx and network errors are retried with exponential backoff. Orders are only
  retried when they carry a `client_order_id`; if Alpaca then reports the id as already used, `submit_order`
  returns the existing order, so a lost response can't turn into a double trade.
- **Pluggable HTTP:** the client talks to a small `Transport.t` record. camelid ships an Eio + cohttp-eio +
  ocaml-tls transport (pure OCaml TLS, system CA bundle); write your own for Lwt or Async, or a fake for tests.

## Install

```bash
opam pin add camelid https://github.com/ka1kqi/camelid.git
```

Requires OCaml 5.1 or newer.

## Quick start

```ocaml
let () =
  Eio_main.run @@ fun env ->
  let creds = Result.get_ok (Camelid.Env.credentials_from_env ()) in  (* APCA_API_KEY_ID, APCA_API_SECRET_KEY *)
  let http = Camelid.Eio_transport.make ~clock:(Eio.Stdenv.clock env) (Eio.Stdenv.net env) in
  let client =
    Camelid.Client.create ~sleep:(Camelid.Eio_transport.sleep (Eio.Stdenv.clock env)) ~env:Paper ~creds http
  in
  match Camelid.Client.account client with
  | Ok a -> Printf.printf "equity $%.2f\n" a.equity
  | Error e -> Format.printf "%a@." Camelid.Client.pp_error e
```

Placing an order:

```ocaml
let req =
  { Camelid.Types.req_symbol = "SPY"; req_side = Buy; amount = Qty 2.5; req_type = Market; tif = Day;
    req_client_order_id = Some "my-strategy-2026-10-22-SPY-buy" }
in
match Camelid.Client.submit_order client req with
| Ok order -> print_endline order.status
| Error e -> Format.printf "%a@." Camelid.Client.pp_error e
```

A deterministic `client_order_id` makes retries and re-runs safe: the same order can't be placed twice, and
`submit_order` hands back the existing order. Then wait for it:

```ocaml
match Camelid.Client.wait_for_order ~timeout:60. client order.id with
| Ok o -> Printf.printf "%s, filled %g\n" o.status o.filled_qty
| Error e -> Format.printf "%a@." Camelid.Client.pp_error e
```

See [`examples/account.ml`](examples/account.ml) for a runnable example (`dune exec examples/account.exe`).

## API

| Function | Endpoint |
| --- | --- |
| `Client.account` | `GET /v2/account` |
| `Client.positions` | `GET /v2/positions` |
| `Client.clock` | `GET /v2/clock` |
| `Client.open_orders` | `GET /v2/orders?status=open` |
| `Client.order` | `GET /v2/orders/{id}` |
| `Client.order_by_client_id` | `GET /v2/orders:by_client_order_id` |
| `Client.submit_order` | `POST /v2/orders` |
| `Client.cancel_order` | `DELETE /v2/orders/{id}` |
| `Client.wait_for_order` | polls `GET /v2/orders/{id}` until filled, canceled, expired or rejected |
| `Client.asset` | `GET /v2/assets/{symbol}` (tradable, fractionable) |
| `Client.latest_stock_prices` | `GET /v2/stocks/trades/latest` (data API) |
| `Client.latest_crypto_prices` | `GET /v1beta3/crypto/us/latest/trades` (data API) |

## Testing without the network

```ocaml
let transport = { Camelid.Transport.request = fun ~meth:_ ~headers:_ ?body:_ uri ->
  match Uri.path uri with
  | "/v2/clock" -> { status = 200; body = {|{"timestamp":"t","is_open":true,"next_open":"o","next_close":"c"}|} }
  | _ -> { status = 404; body = "{}" } }
```

## Development

```bash
opam install . --deps-only --with-test
dune build && dune test
```

On macOS, if the Command Line Tools ship a newer SDK than the running OS, build the switch with
`SDKROOT` pointing at the matching SDK (e.g. `MacOSX26.5.sdk`) and `MACOSX_DEPLOYMENT_TARGET` set to the
OS version. Otherwise OCaml's runtime can pick up spawn functions the OS lacks, and subprocess calls crash.

## Status

Early. Covers what a daily rebalancing bot needs. Not yet covered: streaming (WebSocket), historical bars,
options, account activities. Contributions welcome.

Not affiliated with Alpaca. Trading involves risk; test against the paper API first.

## License

MIT
