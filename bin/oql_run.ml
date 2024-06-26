open Riot

(* open Logger.Make (struct *)
(*     let namespace = [ "bin"; "oql_run" ] *)
(*   end) *)

type query_result = int [@@deriving deserialize]

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
  (* set_log_level (Some Logger.Info); *)
  (* info (fun f -> f "Starting application"); *)
  let* db =
    let config =
      Silo_postgres.config
        ~connections:1
        ~connection_string:
          "postgresql://tjdevries:password@localhost:5432/oql?sslmode=disabled"
    in
    match Silo_postgres.connect ~config with
    | Ok c -> Ok c
    | Error (`Msg e) -> failwith ("connection:" ^ e)
    | _ -> failwith "connection: unknown error"
  in
  let _ = db in
  (* let one = *)
  (*   match *)
  (*     Silo_postgres.query *)
  (*       db *)
  (*       ~query:"select * from twitch_users limit 1" *)
  (*       ~deserializer:deserialize_query_result *)
  (*   with *)
  (*   | Ok one -> one *)
  (*   | Error e -> failwith e *)
  (* in *)
  (* let _ = *)
  (*   match one with *)
  (*   | Some v -> Fmt.pr "@.This is from riot: %d@." v *)
  (*   | None -> Fmt.pr "@.Shouldn't be possible@." *)
  (* in *)
  Fmt.pr "@.This is from riot: %d@." 1;
  (* info (fun f -> f "Starting application"); *)
  (* info (fun f -> f "Starting application"); *)
  (* info (fun f -> f "Starting application"); *)
  (* info (fun f -> f "Starting application"); *)
  Ok 1
;;
