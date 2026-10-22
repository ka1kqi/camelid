(lang dune 3.0)

(name camelid)

(generate_opam_files true)

(license MIT)

(authors "camelid contributors")

(maintainers "camelid contributors")

(source
 (github ka1kqi/camelid))

(package
 (name camelid)
 (synopsis "Typed OCaml client for the Alpaca trading and market-data APIs")
 (description
  "Account, positions, orders, market clock and latest-trade endpoints for Alpaca (paper and live). Pure JSON decoders, a pluggable HTTP transport, and a ready-made Eio + cohttp-eio + ocaml-tls HTTPS transport.")
 (tags (alpaca trading finance brokerage eio))
 (depends
  (ocaml (>= 5.1))
  (dune (>= 3.0))
  (yojson (>= 2.0))
  uri
  domain-name
  (eio (>= 1.0))
  (cohttp-eio (>= 6.0))
  (tls-eio (>= 1.0))
  ca-certs
  mirage-crypto-rng
  (eio_main :with-test)
  (ppx_expect :with-test)))
