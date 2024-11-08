module User = struct
  type t = { id : int } [@@deriving table { name = "users" }]

  include struct
    [@@@ocaml.warning "-60"]

    let _ = fun (_ : t) -> ()

    open! Serde

    let deserialize_t =
      let ( let* ) = Stdlib.Result.bind in
      let _ = ( let* ) in
      let open Serde.De in
      fun ctx ->
        record ctx "t" 1 (fun ctx ->
          let field_visitor =
            let visit_string _ctx str =
              match str with
              | "id" -> Ok `id
              | _ -> Ok `invalid_tag
            in
            let visit_int _ctx str =
              match str with
              | 0 -> Ok `id
              | _ -> Ok `invalid_tag
            in
            Visitor.make ~visit_string ~visit_int ()
          in
          let id = ref None in
          let rec read_fields () =
            let* tag = next_field ctx field_visitor in
            match tag with
            | Some `id ->
              let* v = field ctx "id" int in
              id := Some v;
              read_fields ()
            | Some `invalid_tag ->
              let* () = ignore_any ctx in
              read_fields ()
            | None -> Ok ()
          in
          let* () = read_fields () in
          let* id = Stdlib.Option.to_result ~none:(`Msg "missing field \"id\" (\"id\")") !id in
          Ok { id })
    ;;

    let _ = deserialize_t

    let serialize_t =
      let ( let* ) = Stdlib.Result.bind in
      let _ = ( let* ) in
      let open Serde.Ser in
      fun t ctx ->
        record ctx "t" 1 (fun ctx ->
          let* () = field ctx "id" (int t.id) in
          Ok ())
    ;;

    let _ = serialize_t

    type row = t list [@@deriving serialize, deserialize]

    include struct
      let _ = fun (_ : row) -> ()

      let serialize_row =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.Ser in
        fun t ctx -> (s (list (s serialize_t))) t ctx
      ;;

      let _ = serialize_row

      open! Serde

      let deserialize_row =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.De in
        fun ctx -> (d (list (d deserialize_t))) ctx
      ;;

      let _ = deserialize_row
    end [@@ocaml.doc "@inline"] [@@merlin.hide]

    module Fields = struct
      let id = "id"
      let _ = id

      type id = int [@@deriving deserialize, serialize]

      include struct
        let _ = fun (_ : id) -> ()

        open! Serde

        let deserialize_id =
          let ( let* ) = Stdlib.Result.bind in
          let _ = ( let* ) in
          let open Serde.De in
          fun ctx -> int ctx
        ;;

        let _ = deserialize_id

        let serialize_id =
          let ( let* ) = Stdlib.Result.bind in
          let _ = ( let* ) in
          let open Serde.Ser in
          fun t ctx -> int t ctx
        ;;

        let _ = serialize_id
      end [@@ocaml.doc "@inline"] [@@merlin.hide]
    end

    module Params = struct
      let id id = DBCaml.Params.Values.integer id
      let _ = id
    end

    module Table = struct
      let drop db = DBCaml.execute db ~params:[] ~query:"DROP TABLE IF EXISTS users"
      let _ = drop

      let create db =
        DBCaml.execute db ~params:[] ~query:"CREATE TABLE users (id INTEGER NOT NULL) strict"
      ;;

      let _ = create
    end

    let relation = "users"
    let _ = relation

    let insert ~id db =
      match
        DBCaml.query
          db
          ~params:[ Params.id id ]
          ~query:"INSERT INTO users (id) VALUES (?) RETURNING *"
          ~deserializer:deserialize_row
      with
      | Ok (t :: []) -> Ok t
      | Ok [] -> Error (`msg "empty: Should have returned one item")
      | Ok _ -> Error (`msg "empty: Should not return more than one item")
      | Error err -> Error err
    ;;

    let _ = insert
    let () = Octane.TableRegistry.register { name = "users"; fields = [] }
  end [@@ocaml.doc "@inline"] [@@merlin.hide]
end

module UserByID = struct
  type t =
    { id : User.Fields.id
    ; name : User.Fields.name
    }
  [@@deriving serialize, deserialize]

  include struct
    let _ = fun (_ : t) -> ()

    let serialize_t =
      let ( let* ) = Stdlib.Result.bind in
      let _ = ( let* ) in
      let open Serde.Ser in
      fun t ctx ->
        record ctx "t" 2 (fun ctx ->
          let* () = field ctx "id" ((s User.Fields.serialize_id) t.id) in
          let* () = field ctx "name" ((s User.Fields.serialize_name) t.name) in
          Ok ())
    ;;

    let _ = serialize_t

    open! Serde

    let deserialize_t =
      let ( let* ) = Stdlib.Result.bind in
      let _ = ( let* ) in
      let open Serde.De in
      fun ctx ->
        record ctx "t" 2 (fun ctx ->
          let field_visitor =
            let visit_string _ctx str =
              match str with
              | "name" -> Ok `name
              | "id" -> Ok `id
              | _ -> Ok `invalid_tag
            in
            let visit_int _ctx str =
              match str with
              | 0 -> Ok `name
              | 1 -> Ok `id
              | _ -> Ok `invalid_tag
            in
            Visitor.make ~visit_string ~visit_int ()
          in
          let id = ref None in
          let name = ref None in
          let rec read_fields () =
            let* tag = next_field ctx field_visitor in
            match tag with
            | Some `name ->
              let* v = field ctx "name" (d User.Fields.deserialize_name) in
              name := Some v;
              read_fields ()
            | Some `id ->
              let* v = field ctx "id" (d User.Fields.deserialize_id) in
              id := Some v;
              read_fields ()
            | Some `invalid_tag ->
              let* () = ignore_any ctx in
              read_fields ()
            | None -> Ok ()
          in
          let* () = read_fields () in
          let* id = Stdlib.Option.to_result ~none:(`Msg "missing field \"id\" (\"id\")") !id in
          let* name =
            Stdlib.Option.to_result ~none:(`Msg "missing field \"name\" (\"name\")") !name
          in
          Ok { name; id })
    ;;

    let _ = deserialize_t
  end [@@ocaml.doc "@inline"] [@@merlin.hide]

  module Query = struct
    type query = t list [@@deriving deserialize, serialize]

    include struct
      let _ = fun (_ : query) -> ()

      open! Serde

      let deserialize_query =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.De in
        fun ctx -> (d (list (d deserialize_t))) ctx
      ;;

      let _ = deserialize_query

      let serialize_query =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.Ser in
        fun t ctx -> (s (list (s serialize_t))) t ctx
      ;;

      let _ = serialize_query
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  end

  let deserialize = Query.deserialize_query

  let query db ~id =
    let query =
      Stdlib.Format.sprintf
        "SELECT %s FROM %s WHERE %s"
        (Stdlib.String.concat
           ", "
           [ Stdlib.Format.sprintf "%s.%s" User.relation User.Fields.id
           ; Stdlib.Format.sprintf "%s.%s" User.relation User.Fields.name
           ])
        (String.concat ~sep:", " [ User.relation ])
        (Stdlib.Format.sprintf
           "(%s = %s)"
           (Stdlib.Format.sprintf "%s.%s" User.relation User.Fields.id)
           "$1")
    in
    let open DBCaml.Params.Values in
    let params = [ User.Params.id id ] in
    DBCaml.query db ~query ~params ~deserializer:deserialize
  ;;

  let raw = "SELECT User.id, User.name FROM User WHERE User.id = $id"
end [@warning "-32"]
