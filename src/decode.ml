(* Small JSON helpers. Alpaca sends most numbers as strings ("123.45"), so [number] accepts both. *)

type json = Yojson.Safe.t

exception Error of string

let fail fmt = Printf.ksprintf (fun s -> raise (Error s)) fmt

let member name (json : json) : json =
  match json with
  | `Assoc kv -> ( try List.assoc name kv with Not_found -> `Null)
  | _ -> fail "expected an object when reading %S" name

let to_number name : json -> float = function
  | `String s -> (
      match float_of_string_opt s with Some f -> f | None -> fail "%s: not a number: %S" name s)
  | `Float f -> f
  | `Int i -> float_of_int i
  | `Intlit s -> float_of_string s
  | `Null -> fail "%s: missing" name
  | _ -> fail "%s: expected a number" name

let number name json = to_number name (member name json)

let number_opt name json =
  match member name json with `Null -> None | v -> Some (to_number name v)

let string name json =
  match member name json with `String s -> s | `Null -> fail "%s: missing" name | _ -> fail "%s: expected a string" name

let string_opt name json = match member name json with `String s -> Some s | _ -> None

let bool name json =
  match member name json with `Bool b -> b | `Null -> false | _ -> fail "%s: expected a bool" name

let list f : json -> _ list = function `List l -> List.map f l | `Null -> [] | _ -> fail "expected a list"

let run f (json : json) = match f json with v -> Ok v | exception Error e -> Error e

let parse f body =
  match Yojson.Safe.from_string body with
  | json -> run f json
  | exception Yojson.Json_error e -> Error ("invalid JSON: " ^ e)
