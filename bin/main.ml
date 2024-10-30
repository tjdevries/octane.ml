open Riot
open Core

open Logger.Make (struct
    let namespace = [ "examples"; "basic_postgres" ]
  end)

let ( let* ) = Stdlib.Result.bind

module User = struct
  type t =
    { id : int
    ; name : string
    ; age : int
    }
  [@@deriving table { name = "users" }]
end

let%query (module UserNames) =
  {| SELECT User.id, User.name, User.age FROM User WHERE User.name = $id |}
;;

(* Start the database connection pool *)

let _ =
  Riot.run_with_status ~on_error:(fun x -> failwith x)
  @@ fun () ->
  let _ = Logger.start () |> Result.ok in
  set_log_level (Some Trace);
  info (fun f -> f "Starting application");
  let* db =
    let config =
      Silo.config
        ~connections:1
        ~driver:(module Dbcaml_driver_postgres)
        ~connection_string:
          "postgresql://tjdevries:password@omen:5432/oql?sslmode=disable"
    in
    Silo.connect ~config
  in
  let* _ = UserNames.query db ~id:"teej_dv" in
  (* List.iter *)
  (*   ~f:(fun { id; name } -> Fmt.pr "This is from riot: %d - %s@." id name) *)
  (*   users; *)
  (* Fetch the user and return the user to a variable *)
  (* let* fetched_users = *)
  (*   Silo.query *)
  (*     db *)
  (*     ~query: *)
  (*       "select name, id, some_bool, pet_name, some_int64, some_int32, \ *)
           (*        some_float, pets, pets as pets_array from users limit 2" *)
  (*     ~deserializer:deserialize_users *)
  (* in *)
  (* List.iter *)
  (*   (fun x -> *)
  (*     Printf.printf *)
  (*       "Fetching user with id %d:\n\ *)
           (*        Name: %s\n\ *)
           (*        Some float: %f\n\ *)
           (*        Some int64: %d\n\ *)
           (*        Some int32: %d\n\ *)
           (*        %s\n\ *)
           (*       \ Some bool: %b\n\ *)
           (*        Pets: %s\n\ *)
           (*        Pets array: %s\n\n" *)
  (*       x.id *)
  (*       x.name *)
  (*       x.some_float *)
  (*       (Int64.to_int x.some_int64) *)
  (*       (Int32.to_int x.some_int32) *)
  (*       (match x.pet_name with *)
  (*        | Some pn -> Printf.sprintf "Pet name: %S" pn *)
  (*        | None -> "No pet") *)
  (*       x.some_bool *)
  (*       (String.concat ", " x.pets) *)
  (*       (String.concat ", " (Array.to_list x.pets_array))) *)
  (*   (Option.get fetched_users); *)
  Ok 0
;;
