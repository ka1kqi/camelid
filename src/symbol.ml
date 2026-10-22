(* Alpaca writes crypto pairs as "BTC/USD" in orders and market data but "BTCUSD" in positions.
   [canonical] maps both forms to the slash form so callers can compare symbols safely. *)

let quote_currencies = [ "USD"; "USDT"; "USDC"; "BTC" ]

let canonical ~asset_class sym =
  if asset_class <> "crypto" || String.contains sym '/' then sym
  else
    match
      List.find_opt
        (fun q ->
          let lq = String.length q and ls = String.length sym in
          ls > lq && String.sub sym (ls - lq) lq = q)
        quote_currencies
    with
    | Some q ->
        let base = String.sub sym 0 (String.length sym - String.length q) in
        base ^ "/" ^ q
    | None -> sym
