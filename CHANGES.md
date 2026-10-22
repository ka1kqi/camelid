## Unreleased

- Retries with exponential backoff for 429, 5xx and network errors; orders retried only with a
  `client_order_id`, and a duplicate-id rejection returns the existing order.
- Request timeout in `Eio_transport.make ?timeout ~clock` (default 30s); network errors become
  `Client.Network` instead of exceptions. `make` no longer takes `~sw`.
- New: `Client.order`, `order_by_client_id`, `wait_for_order`, `asset`; `Types.is_final`.

- Initial version: account, positions, orders (submit, list open, cancel), clock, latest stock and
  crypto trades; paper and live endpoints; pluggable transport with an Eio + cohttp-eio + ocaml-tls
  implementation.
