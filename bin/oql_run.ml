open Riot

let ( let* ) = Result.bind

open Logger.Make (struct
    let namespace = [ "bin"; "oql_run" ]
  end)

module User = struct
  type t =
    { id : int
    ; name : string
    }
  [@@deriving table { name = "users" }]
end

let%query (module UserName) = "select User.id, User.name from User"

let () =
  Riot.run_with_status ~workers:2 ~on_error:(fun x -> failwith x)
  @@ fun () ->
  let _ =
    match Logger.start () with
    | Error (`Msg e) -> failwith e
    | Error `Supervisor_error -> failwith "SUPERVISOR"
    | Error (`Application_error msg) -> failwith msg
    | Ok pid -> pid
  in
  set_log_level (Some Logger.Trace);
  info (fun f -> f "Starting application");
  let* db =
    let config =
      Silo_postgres.config
        ~connections:2
        ~connection_string:
          "postgresql://tjdevries:password@localhosting:5434/oql?sslmode=disable"
    in
    match Silo_postgres.connect ~config with
    | Ok c -> Ok c
    | Error (`Msg e) -> failwith ("connection:" ^ e)
    | _ -> failwith "connection: unknown error"
  in
  let _ = db in
  let one =
    match
      Silo_postgres.query
        db
        ~query:"select id, name from users limit 1"
        ~deserializer:UserName.deserialize_t
    with
    | Ok one -> one
    | Error e -> failwith e
  in
  (* let user_name = UserNamesAndAges.query db in *)
  let _ =
    match one with
    | Some { id; name } -> Fmt.pr "@.This is from riot: %d - %s@." id name
    | None -> Fmt.pr "@.Shouldn't be possible@."
  in
  Fmt.pr "@.This is from riot: %d@." 1;
  (* info (fun f -> f "Starting application"); *)
  (* info (fun f -> f "Starting application"); *)
  (* info (fun f -> f "Starting application"); *)
  (* info (fun f -> f "Starting application"); *)
  Ok 1
;;
