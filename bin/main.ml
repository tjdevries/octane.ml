open Octane.Schema4

let () =
  Format.printf "Hello, World!@.";
  (* Format.printf "User    Request:@.%s@.@." (build_request user_query); *)
  Format.printf "Spelled Request:@.%s@.@." (build_request spelled_out);
  Format.printf "Query   Request:@.%s@.@." (build_request query);
  ()
;;

let () =
  Format.printf "Schema 5!@.";
  (* Format.printf "User    Request:@.%s@.@." (build_request user_query); *)
  Format.printf
    "Spelled Request:@.%s@.@."
    (Octane.Schema5.print_query Octane.Schema5.query);
  ()
;;

let connect () =
  let uri = "sqlite3:///home/tjdevries/git/octane/db.sqlite" in
  let connection = Caqti_blocking.connect (Uri.of_string uri) in
  match connection with
  | Ok conn -> conn
  | Error err ->
    failwith
      (Format.asprintf "Error connecting to %s: %a@." uri Caqti_error.pp err)
;;

(* module `Q` contains our query definitions *)
module Q = struct
  open Caqti_request.Infix

  (*
     Caqti infix operators

     ->! decodes a single row
     ->? decodes zero or one row
     ->* decodes many rows
     ->. expects no row
  *)

  (* `add` takes 2 ints (as a tuple), and returns 1 int *)
  let add =
    Caqti_type.(t2 int int ->! int)
    "SELECT ? + ?"
  [@@ocamlformat "disable"]

  let mul =
    Caqti_type.(t2 int int ->! int)
    "SELECT ? * ?"
  [@@ocamlformat "disable"]

  let result x = Caqti_type.(t2 int x ->! int) "SELECT ? * ? * 2"
end

let add a b conn =
  let module Conn = (val conn : Caqti_blocking.CONNECTION) in
  Conn.find Q.add (a, b)
;;

let mul a b (module Conn : Caqti_blocking.CONNECTION) = Conn.find Q.mul (a, b)

let testing x a b (module Conn : Caqti_blocking.CONNECTION) =
  Conn.find (Q.result x) (a, b)
;;

let () =
  let conn = connect () in
  let res1 = mul 5 10 conn in
  (* Ok... so possible to construct a caqti query here. *)
  let res2 = testing Octane.Schema_caqti.caqti_thing 5 10 conn in
  (* Ok, cool, we can connect to simple sql db *)
  (* ... So now the question is... can I build the query dynamically as well *)
  let _ =
    match res1, res2 with
    | Ok n1, Ok n2 -> Format.printf "%d, %d@." n1 n2
    | _ -> assert false
  in
  ()
;;
