open Base
open Riot

let ( let* ) = Stdlib.Result.bind

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

let%query (module UserName) = "SELECT  User.name, User.id FROM User"

let example db =
  let* users = UserName.query db in
  let users = Option.value users ~default:[] in
  List.iter users ~f:(fun { id; name } ->
    Fmt.pr "@.We read this from the database: %d - %s@." id name);
  Ok ()
;;

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
  (* set_log_level (Some Logger.Trace); *)
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
  let users =
    match UserName.query db with
    | Ok one -> one
    | Error e -> failwith e
  in
  let _ =
    match users with
    | Some users ->
      List.iter
        ~f:(fun { id; name } -> Fmt.pr "@.This is from riot: %d - %s@." id name)
        users
    | _ -> Fmt.pr "@.Shouldn't be possible@."
  in
  Ok 1
;;
