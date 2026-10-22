(* The client only needs "send a request, get status and body". Keeping that behind a record lets tests
   (and users on Lwt/Async) plug in their own HTTP stack.

   [request] may raise on network failures or timeouts; the client turns any exception into
   [Client.Network]. *)

type meth = [ `GET | `POST | `DELETE ]

type response = { status : int; body : string }

type t = { request : meth:meth -> headers:(string * string) list -> ?body:string -> Uri.t -> response }
