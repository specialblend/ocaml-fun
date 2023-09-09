let ( ->. ) = ( |> )
let ( ->: ) f g x = g (f x)

let ( ->? ) opt default =
  match opt with
  | Some x -> x
  | None -> default

let try_wrap task =
  try Ok (task ()) with
  | e -> Error e

let try_catch fn task =
  try Ok (task ()) with
  | e -> Error (fn e)

let fail = Printexc.to_string

let bye msg =
  output_string stdout msg;
  flush stdout;
  exit 0

let die msg =
  output_string stderr msg;
  flush stderr;
  exit 1

let between (a, b) x = x >= a && x < b
let clamp (lower, upper) n = max lower (min upper n)

module List' = struct
  let head = function
    | h :: _ -> Some h
    | _ -> None

  let flat_map = List.concat_map
  let map_ignore fn = List.iter (fn ->: ignore)
  let reject fn l = List.filter (fn ->: not) l

  let tap fn l =
    List.iter (fn ->: ignore) l;
    l

  let touch default = function
    | [] -> default
    | l -> l

  let touch_with fn = function
    | [] -> fn ()
    | l -> l

  let unwrap default = function
    | [] -> default
    | h :: _ -> h

  let ( ->? ) t default = unwrap default t
end

module Option' = struct
  let flat_map f t = Option.bind t f

  let tap fn = function
    | None -> None
    | Some x ->
        ignore (fn x);
        Some x

  let touch default = function
    | Some x -> Some x
    | None -> Some default

  let touch_with fn = function
    | Some x -> Some x
    | None -> fn ()

  let unwrap default = function
    | Some x -> x
    | None -> default

  let ( ->? ) t default = unwrap default t
end

module Result' = struct
  let flat_map f t = Result.bind t f

  let tap fn = function
    | Error e -> Error e
    | Ok x ->
        ignore (fn x);
        Ok x

  let tap_err fn = function
    | Ok x -> Ok x
    | Error e ->
        ignore (fn e);
        Error e

  let touch default = function
    | Ok x -> Ok x
    | Error _ -> Ok default

  let touch_with fn = function
    | Ok x -> Ok x
    | Error _ -> fn ()

  let ( ->? ) t default =
    match t with
    | Ok x -> x
    | Error _ -> default
end