Pretty print the file

  $ pp_query ./lib/table.ml > ./lib/table-generated.ml
  $ ocamlformat ./lib/table-generated.ml
  module User = struct
    type t =
      { id : int [@primary_key { autoincrement = true }]
      ; name : string
      ; age : int
      }
    [@@deriving table { name = "users" }]
  
    include struct
      [@@@ocaml.warning "-60"]
  
      let _ = fun (_ : t) -> ()
  
      open! Serde
  
      let deserialize_t =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.De in
        fun ctx ->
          record ctx "t" 3 (fun ctx ->
            let field_visitor =
              let visit_string _ctx str =
                match str with
                | "age" -> Ok `age
                | "name" -> Ok `name
                | "id" -> Ok `id
                | _ -> Ok `invalid_tag
              in
              let visit_int _ctx str =
                match str with
                | 0 -> Ok `age
                | 1 -> Ok `name
                | 2 -> Ok `id
                | _ -> Ok `invalid_tag
              in
              Visitor.make ~visit_string ~visit_int ()
            in
            let id = ref None in
            let name = ref None in
            let age = ref None in
            let rec read_fields () =
              let* tag = next_field ctx field_visitor in
              match tag with
              | Some `age ->
                let* v = field ctx "age" int in
                age := Some v;
                read_fields ()
              | Some `name ->
                let* v = field ctx "name" string in
                name := Some v;
                read_fields ()
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
            let* name = Stdlib.Option.to_result ~none:(`Msg "missing field \"name\" (\"name\")") !name in
            let* age = Stdlib.Option.to_result ~none:(`Msg "missing field \"age\" (\"age\")") !age in
            Ok { age; name; id })
      ;;
  
      let _ = deserialize_t
  
      let serialize_t =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.Ser in
        fun t ctx ->
          record ctx "t" 3 (fun ctx ->
            let* () = field ctx "id" (int t.id) in
            let* () = field ctx "name" (string t.name) in
            let* () = field ctx "age" (int t.age) in
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
        let name = "name"
        let _ = name
        let age = "age"
        let _ = age
  
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
  
        type name = string [@@deriving deserialize, serialize]
  
        include struct
          let _ = fun (_ : name) -> ()
  
          open! Serde
  
          let deserialize_name =
            let ( let* ) = Stdlib.Result.bind in
            let _ = ( let* ) in
            let open Serde.De in
            fun ctx -> string ctx
          ;;
  
          let _ = deserialize_name
  
          let serialize_name =
            let ( let* ) = Stdlib.Result.bind in
            let _ = ( let* ) in
            let open Serde.Ser in
            fun t ctx -> string t ctx
          ;;
  
          let _ = serialize_name
        end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
        type age = int [@@deriving deserialize, serialize]
  
        include struct
          let _ = fun (_ : age) -> ()
  
          open! Serde
  
          let deserialize_age =
            let ( let* ) = Stdlib.Result.bind in
            let _ = ( let* ) in
            let open Serde.De in
            fun ctx -> int ctx
          ;;
  
          let _ = deserialize_age
  
          let serialize_age =
            let ( let* ) = Stdlib.Result.bind in
            let _ = ( let* ) in
            let open Serde.Ser in
            fun t ctx -> int t ctx
          ;;
  
          let _ = serialize_age
        end [@@ocaml.doc "@inline"] [@@merlin.hide]
      end
  
      module Params = struct
        let id id = DBCaml.Params.Values.integer id
        let _ = id
        let name name = DBCaml.Params.Values.text name
        let _ = name
        let age age = DBCaml.Params.Values.integer age
        let _ = age
      end
  
      module Table = struct
        let drop db = DBCaml.execute db ~params:[] ~query:"DROP TABLE IF EXISTS users"
        let _ = drop
  
        let create db =
          DBCaml.execute
            db
            ~params:[]
            ~query:
              "CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL, age INTEGER NOT NULL) strict"
        ;;
  
        let _ = create
      end
  
      let relation = "users"
      let _ = relation
  
      let insert ~name ~age db =
        match
          DBCaml.query
            db
            ~params:[ Params.name name; Params.age age ]
            ~query:"INSERT INTO users (name, age) VALUES (?, ?) RETURNING *"
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
  
  module UserNameQuery = struct
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
            let* name = Stdlib.Option.to_result ~none:(`Msg "missing field \"name\" (\"name\")") !name in
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
  
    let query db =
      let query =
        Stdlib.Format.sprintf
          "SELECT %s FROM %s"
          (Stdlib.String.concat
             ", "
             [ Stdlib.Format.sprintf "%s.%s" User.relation User.Fields.id
             ; Stdlib.Format.sprintf "%s.%s" User.relation User.Fields.name
             ])
          (String.concat ~sep:", " [ User.relation ])
      in
      let open DBCaml.Params.Values in
      let params = [] in
      DBCaml.query db ~query ~params ~deserializer:deserialize
    ;;
  
    let raw = "select User.id, User.name from User"
  end [@warning "-32"]
< language: ocaml

  $ pp_query ./lib/where_id.ml | ocamlformat --impl -
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
        let create db = DBCaml.execute db ~params:[] ~query:"CREATE TABLE users (id INTEGER NOT NULL) strict"
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
            let* name = Stdlib.Option.to_result ~none:(`Msg "missing field \"name\" (\"name\")") !name in
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
          (Stdlib.Format.sprintf "(%s = %s)" (Stdlib.Format.sprintf "%s.%s" User.relation User.Fields.id) "$1")
      in
      let open DBCaml.Params.Values in
      let params = [ User.Params.id id ] in
      DBCaml.query db ~query ~params ~deserializer:deserialize
    ;;
  
    let raw = "SELECT User.id, User.name FROM User WHERE User.id = $id"
  end [@warning "-32"]
< language: ocaml

  $ pp_query ./lib/where_positional.ml | ocamlformat --impl -
  module User = struct
    type t =
      { id : int
      ; name : string
      }
    [@@deriving table { name = "users" }]
  
    include struct
      [@@@ocaml.warning "-60"]
  
      let _ = fun (_ : t) -> ()
  
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
                let* v = field ctx "name" string in
                name := Some v;
                read_fields ()
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
            let* name = Stdlib.Option.to_result ~none:(`Msg "missing field \"name\" (\"name\")") !name in
            Ok { name; id })
      ;;
  
      let _ = deserialize_t
  
      let serialize_t =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.Ser in
        fun t ctx ->
          record ctx "t" 2 (fun ctx ->
            let* () = field ctx "id" (int t.id) in
            let* () = field ctx "name" (string t.name) in
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
        let name = "name"
        let _ = name
  
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
  
        type name = string [@@deriving deserialize, serialize]
  
        include struct
          let _ = fun (_ : name) -> ()
  
          open! Serde
  
          let deserialize_name =
            let ( let* ) = Stdlib.Result.bind in
            let _ = ( let* ) in
            let open Serde.De in
            fun ctx -> string ctx
          ;;
  
          let _ = deserialize_name
  
          let serialize_name =
            let ( let* ) = Stdlib.Result.bind in
            let _ = ( let* ) in
            let open Serde.Ser in
            fun t ctx -> string t ctx
          ;;
  
          let _ = serialize_name
        end [@@ocaml.doc "@inline"] [@@merlin.hide]
      end
  
      module Params = struct
        let id id = DBCaml.Params.Values.integer id
        let _ = id
        let name name = DBCaml.Params.Values.text name
        let _ = name
      end
  
      module Table = struct
        let drop db = DBCaml.execute db ~params:[] ~query:"DROP TABLE IF EXISTS users"
        let _ = drop
  
        let create db =
          DBCaml.execute db ~params:[] ~query:"CREATE TABLE users (id INTEGER NOT NULL, name TEXT NOT NULL) strict"
        ;;
  
        let _ = create
      end
  
      let relation = "users"
      let _ = relation
  
      let insert ~id ~name db =
        match
          DBCaml.query
            db
            ~params:[ Params.id id; Params.name name ]
            ~query:"INSERT INTO users (id, name) VALUES (?, ?) RETURNING *"
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
    type t = { name : User.Fields.name } [@@deriving serialize, deserialize]
  
    include struct
      let _ = fun (_ : t) -> ()
  
      let serialize_t =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.Ser in
        fun t ctx ->
          record ctx "t" 1 (fun ctx ->
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
          record ctx "t" 1 (fun ctx ->
            let field_visitor =
              let visit_string _ctx str =
                match str with
                | "name" -> Ok `name
                | _ -> Ok `invalid_tag
              in
              let visit_int _ctx str =
                match str with
                | 0 -> Ok `name
                | _ -> Ok `invalid_tag
              in
              Visitor.make ~visit_string ~visit_int ()
            in
            let name = ref None in
            let rec read_fields () =
              let* tag = next_field ctx field_visitor in
              match tag with
              | Some `name ->
                let* v = field ctx "name" (d User.Fields.deserialize_name) in
                name := Some v;
                read_fields ()
              | Some `invalid_tag ->
                let* () = ignore_any ctx in
                read_fields ()
              | None -> Ok ()
            in
            let* () = read_fields () in
            let* name = Stdlib.Option.to_result ~none:(`Msg "missing field \"name\" (\"name\")") !name in
            Ok { name })
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
  
    let query db p1 p2 =
      let query =
        Stdlib.Format.sprintf
          "SELECT %s FROM %s WHERE %s"
          (Stdlib.String.concat ", " [ Stdlib.Format.sprintf "%s.%s" User.relation User.Fields.name; p2 ])
          (String.concat ~sep:", " [ User.relation ])
          "TODO"
      in
      let open DBCaml.Params.Values in
      let params = [ p1; p2 ] in
      DBCaml.query db ~query ~params ~deserializer:deserialize
    ;;
  
    let raw = "SELECT User.name, $2 FROM User WHERE User.id = $1"
  end [@warning "-32"]
< language: ocaml

  $ pp_query ./lib/foreign.ml | ocamlformat --impl -
  module User = struct
    type t =
      { id : int [@primary_key { autoincrement = true }]
      ; name : string
      }
    [@@deriving table { name = "users" }]
  
    include struct
      [@@@ocaml.warning "-60"]
  
      let _ = fun (_ : t) -> ()
  
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
                let* v = field ctx "name" string in
                name := Some v;
                read_fields ()
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
            let* name = Stdlib.Option.to_result ~none:(`Msg "missing field \"name\" (\"name\")") !name in
            Ok { name; id })
      ;;
  
      let _ = deserialize_t
  
      let serialize_t =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.Ser in
        fun t ctx ->
          record ctx "t" 2 (fun ctx ->
            let* () = field ctx "id" (int t.id) in
            let* () = field ctx "name" (string t.name) in
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
        let name = "name"
        let _ = name
  
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
  
        type name = string [@@deriving deserialize, serialize]
  
        include struct
          let _ = fun (_ : name) -> ()
  
          open! Serde
  
          let deserialize_name =
            let ( let* ) = Stdlib.Result.bind in
            let _ = ( let* ) in
            let open Serde.De in
            fun ctx -> string ctx
          ;;
  
          let _ = deserialize_name
  
          let serialize_name =
            let ( let* ) = Stdlib.Result.bind in
            let _ = ( let* ) in
            let open Serde.Ser in
            fun t ctx -> string t ctx
          ;;
  
          let _ = serialize_name
        end [@@ocaml.doc "@inline"] [@@merlin.hide]
      end
  
      module Params = struct
        let id id = DBCaml.Params.Values.integer id
        let _ = id
        let name name = DBCaml.Params.Values.text name
        let _ = name
      end
  
      module Table = struct
        let drop db = DBCaml.execute db ~params:[] ~query:"DROP TABLE IF EXISTS users"
        let _ = drop
  
        let create db =
          DBCaml.execute
            db
            ~params:[]
            ~query:"CREATE TABLE users (id INTEGER PRIMARY KEY AUTOINCREMENT, name TEXT NOT NULL) strict"
        ;;
  
        let _ = create
      end
  
      let relation = "users"
      let _ = relation
  
      let insert ~name db =
        match
          DBCaml.query
            db
            ~params:[ Params.name name ]
            ~query:"INSERT INTO users (name) VALUES (?) RETURNING *"
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
  
  module Post = struct
    type t =
      { id : int [@primary_key { autoincrement = true }]
      ; user_id : int [@references User.id { on_delete = Cascade }]
      ; content : string
      }
    [@@deriving table { name = "posts" }]
  
    include struct
      [@@@ocaml.warning "-60"]
  
      let _ = fun (_ : t) -> ()
  
      open! Serde
  
      let deserialize_t =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.De in
        fun ctx ->
          record ctx "t" 3 (fun ctx ->
            let field_visitor =
              let visit_string _ctx str =
                match str with
                | "content" -> Ok `content
                | "user_id" -> Ok `user_id
                | "id" -> Ok `id
                | _ -> Ok `invalid_tag
              in
              let visit_int _ctx str =
                match str with
                | 0 -> Ok `content
                | 1 -> Ok `user_id
                | 2 -> Ok `id
                | _ -> Ok `invalid_tag
              in
              Visitor.make ~visit_string ~visit_int ()
            in
            let id = ref None in
            let user_id = ref None in
            let content = ref None in
            let rec read_fields () =
              let* tag = next_field ctx field_visitor in
              match tag with
              | Some `content ->
                let* v = field ctx "content" string in
                content := Some v;
                read_fields ()
              | Some `user_id ->
                let* v = field ctx "user_id" int in
                user_id := Some v;
                read_fields ()
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
            let* user_id = Stdlib.Option.to_result ~none:(`Msg "missing field \"user_id\" (\"user_id\")") !user_id in
            let* content = Stdlib.Option.to_result ~none:(`Msg "missing field \"content\" (\"content\")") !content in
            Ok { content; user_id; id })
      ;;
  
      let _ = deserialize_t
  
      let serialize_t =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.Ser in
        fun t ctx ->
          record ctx "t" 3 (fun ctx ->
            let* () = field ctx "id" (int t.id) in
            let* () = field ctx "user_id" (int t.user_id) in
            let* () = field ctx "content" (string t.content) in
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
        let user_id = "user_id"
        let _ = user_id
        let content = "content"
        let _ = content
  
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
  
        type user_id = int [@@deriving deserialize, serialize]
  
        include struct
          let _ = fun (_ : user_id) -> ()
  
          open! Serde
  
          let deserialize_user_id =
            let ( let* ) = Stdlib.Result.bind in
            let _ = ( let* ) in
            let open Serde.De in
            fun ctx -> int ctx
          ;;
  
          let _ = deserialize_user_id
  
          let serialize_user_id =
            let ( let* ) = Stdlib.Result.bind in
            let _ = ( let* ) in
            let open Serde.Ser in
            fun t ctx -> int t ctx
          ;;
  
          let _ = serialize_user_id
        end [@@ocaml.doc "@inline"] [@@merlin.hide]
  
        type content = string [@@deriving deserialize, serialize]
  
        include struct
          let _ = fun (_ : content) -> ()
  
          open! Serde
  
          let deserialize_content =
            let ( let* ) = Stdlib.Result.bind in
            let _ = ( let* ) in
            let open Serde.De in
            fun ctx -> string ctx
          ;;
  
          let _ = deserialize_content
  
          let serialize_content =
            let ( let* ) = Stdlib.Result.bind in
            let _ = ( let* ) in
            let open Serde.Ser in
            fun t ctx -> string t ctx
          ;;
  
          let _ = serialize_content
        end [@@ocaml.doc "@inline"] [@@merlin.hide]
      end
  
      module Params = struct
        let id id = DBCaml.Params.Values.integer id
        let _ = id
        let user_id user_id = DBCaml.Params.Values.integer user_id
        let _ = user_id
        let content content = DBCaml.Params.Values.text content
        let _ = content
      end
  
      module Table = struct
        let drop db = DBCaml.execute db ~params:[] ~query:"DROP TABLE IF EXISTS posts"
        let _ = drop
  
        let create db =
          DBCaml.execute
            db
            ~params:[]
            ~query:
              "CREATE TABLE posts (id INTEGER PRIMARY KEY AUTOINCREMENT, user_id INTEGER REFERENCES users (id) ON DELETE \
               CASCADE, content TEXT NOT NULL) strict"
        ;;
  
        let _ = create
      end
  
      let relation = "posts"
      let _ = relation
  
      let insert ~content db =
        match
          DBCaml.query
            db
            ~params:[ Params.content content ]
            ~query:"INSERT INTO posts (content) VALUES (?) RETURNING *"
            ~deserializer:deserialize_row
        with
        | Ok (t :: []) -> Ok t
        | Ok [] -> Error (`msg "empty: Should have returned one item")
        | Ok _ -> Error (`msg "empty: Should not return more than one item")
        | Error err -> Error err
      ;;
  
      let _ = insert
      let () = Octane.TableRegistry.register { name = "posts"; fields = [] }
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  end
< language: ocaml

  $ pp_query ./lib/invalid_model.ml | ocamlformat --impl -
  module ShouldError = struct
    type t = [%ocaml.error "Invalid Model: Module 'Post' is not selected in query"]
  
    let raw = "SELECT Post.id from User"
  end [@warning "-32"]
< language: ocaml

  $ pp_query ./lib/simple_join.ml | ocamlformat --impl -
  module AuthorAndContent = struct
    type t =
      { name : User.Fields.name
      ; content : Post.Fields.content
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
            let* () = field ctx "name" ((s User.Fields.serialize_name) t.name) in
            let* () = field ctx "content" ((s Post.Fields.serialize_content) t.content) in
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
                | "content" -> Ok `content
                | "name" -> Ok `name
                | _ -> Ok `invalid_tag
              in
              let visit_int _ctx str =
                match str with
                | 0 -> Ok `content
                | 1 -> Ok `name
                | _ -> Ok `invalid_tag
              in
              Visitor.make ~visit_string ~visit_int ()
            in
            let name = ref None in
            let content = ref None in
            let rec read_fields () =
              let* tag = next_field ctx field_visitor in
              match tag with
              | Some `content ->
                let* v = field ctx "content" (d Post.Fields.deserialize_content) in
                content := Some v;
                read_fields ()
              | Some `name ->
                let* v = field ctx "name" (d User.Fields.deserialize_name) in
                name := Some v;
                read_fields ()
              | Some `invalid_tag ->
                let* () = ignore_any ctx in
                read_fields ()
              | None -> Ok ()
            in
            let* () = read_fields () in
            let* name = Stdlib.Option.to_result ~none:(`Msg "missing field \"name\" (\"name\")") !name in
            let* content = Stdlib.Option.to_result ~none:(`Msg "missing field \"content\" (\"content\")") !content in
            Ok { content; name })
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
  
    let query db =
      let query =
        Stdlib.Format.sprintf
          "SELECT %s FROM %s"
          (Stdlib.String.concat
             ", "
             [ Stdlib.Format.sprintf "%s.%s" User.relation User.Fields.name
             ; Stdlib.Format.sprintf "%s.%s" Post.relation Post.Fields.content
             ])
          (Stdlib.Format.sprintf
             "%s %s"
             Post.relation
             (String.concat
                ~sep:"\n"
                [ Stdlib.Format.sprintf
                    "%s %s ON %s"
                    "INNER JOIN"
                    User.relation
                    (Stdlib.Format.sprintf
                       "(%s = %s)"
                       (Stdlib.Format.sprintf "%s.%s" User.relation User.Fields.id)
                       (Stdlib.Format.sprintf "%s.%s" Post.relation Post.Fields.author))
                ]))
      in
      let open DBCaml.Params.Values in
      let params = [] in
      DBCaml.query db ~query ~params ~deserializer:deserialize
    ;;
  
    let raw = " SELECT User.name, Post.content FROM Post INNER JOIN User ON User.id = Post.author "
  end [@warning "-32"]
  
  module AuthorAndContent = struct
    type t =
      { name : User.Fields.name
      ; content : Post.Fields.content
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
            let* () = field ctx "name" ((s User.Fields.serialize_name) t.name) in
            let* () = field ctx "content" ((s Post.Fields.serialize_content) t.content) in
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
                | "content" -> Ok `content
                | "name" -> Ok `name
                | _ -> Ok `invalid_tag
              in
              let visit_int _ctx str =
                match str with
                | 0 -> Ok `content
                | 1 -> Ok `name
                | _ -> Ok `invalid_tag
              in
              Visitor.make ~visit_string ~visit_int ()
            in
            let name = ref None in
            let content = ref None in
            let rec read_fields () =
              let* tag = next_field ctx field_visitor in
              match tag with
              | Some `content ->
                let* v = field ctx "content" (d Post.Fields.deserialize_content) in
                content := Some v;
                read_fields ()
              | Some `name ->
                let* v = field ctx "name" (d User.Fields.deserialize_name) in
                name := Some v;
                read_fields ()
              | Some `invalid_tag ->
                let* () = ignore_any ctx in
                read_fields ()
              | None -> Ok ()
            in
            let* () = read_fields () in
            let* name = Stdlib.Option.to_result ~none:(`Msg "missing field \"name\" (\"name\")") !name in
            let* content = Stdlib.Option.to_result ~none:(`Msg "missing field \"content\" (\"content\")") !content in
            Ok { content; name })
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
  
    let query db =
      let query =
        Stdlib.Format.sprintf
          "SELECT %s FROM %s"
          (Stdlib.String.concat
             ", "
             [ Stdlib.Format.sprintf "%s.%s" User.relation User.Fields.name
             ; Stdlib.Format.sprintf "%s.%s" Post.relation Post.Fields.content
             ])
          (Stdlib.Format.sprintf
             "%s %s"
             Post.relation
             (String.concat
                ~sep:"\n"
                [ Stdlib.Format.sprintf
                    "%s %s ON %s"
                    "INNER JOIN"
                    User.relation
                    (Stdlib.Format.sprintf
                       "(%s = %s)"
                       (Stdlib.Format.sprintf "%s.%s" User.relation User.Fields.id)
                       (Stdlib.Format.sprintf "%s.%s" Post.relation Post.Fields.authorasdf))
                ]))
      in
      let open DBCaml.Params.Values in
      let params = [] in
      DBCaml.query db ~query ~params ~deserializer:deserialize
    ;;
  
    let raw = " SELECT User.name, Post.content FROM Post INNER JOIN User ON User.id = Post.authorasdf "
  end [@warning "-32"]
< language: ocaml

  $ pp_query ./lib/missing_name.ml | ocamlformat --impl -
  module User = struct
    type t =
      { id : int [@primary_key { autoincrement = true }]
      ; name : string
      ; middle_name : string option
      }
    [@@deriving table { named = "users" }]
  
    include struct
      let _ = fun (_ : t) -> ()
  
      [%%ocaml.error "Ppxlib.Deriving: generator 'table' doesn't accept argument 'named'.\nHint: Did you mean name?"]
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  end
< language: ocaml

  $ pp_query ./lib/error__multiple_types.ml | ocamlformat --impl -
  File "./lib/error__multiple_types.ml", lines 2-8, characters 2-60:
  2 | ..type t =
  3 |     { id : int [@primary_key { autoincrement = true }]
  4 |     ; name : string
  5 |     ; middle_name : string option
  6 |     }
  7 | 
  8 |   and x = { id : int } [@@deriving table { name = "users" }]
  Error: ppx_table requires exactly one type declaration
< language: ocaml

  $ pp_query ./lib/optional_field.ml | ocamlformat --impl -
  module OptionalField = struct
    type t =
      { id : int [@primary_key { autoincrement = true }]
      ; optional : string option
      }
    [@@deriving table { name = "optional_field" }]
  
    include struct
      [@@@ocaml.warning "-60"]
  
      let _ = fun (_ : t) -> ()
  
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
                | "optional" -> Ok `optional
                | "id" -> Ok `id
                | _ -> Ok `invalid_tag
              in
              let visit_int _ctx str =
                match str with
                | 0 -> Ok `optional
                | 1 -> Ok `id
                | _ -> Ok `invalid_tag
              in
              Visitor.make ~visit_string ~visit_int ()
            in
            let id = ref None in
            let optional = ref None in
            let rec read_fields () =
              let* tag = next_field ctx field_visitor in
              match tag with
              | Some `optional ->
                let* v = field ctx "optional" (d (option string)) in
                optional := Some v;
                read_fields ()
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
            let optional =
              match !optional with
              | Some opt -> opt
              | None -> None
            in
            Ok { optional; id })
      ;;
  
      let _ = deserialize_t
  
      let serialize_t =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.Ser in
        fun t ctx ->
          record ctx "t" 2 (fun ctx ->
            let* () = field ctx "id" (int t.id) in
            let* () = field ctx "optional" ((s (option string)) t.optional) in
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
        let optional = "optional"
        let _ = optional
  
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
  
        type optional = string option [@@deriving deserialize, serialize]
  
        include struct
          let _ = fun (_ : optional) -> ()
  
          open! Serde
  
          let deserialize_optional =
            let ( let* ) = Stdlib.Result.bind in
            let _ = ( let* ) in
            let open Serde.De in
            fun ctx -> (d (option string)) ctx
          ;;
  
          let _ = deserialize_optional
  
          let serialize_optional =
            let ( let* ) = Stdlib.Result.bind in
            let _ = ( let* ) in
            let open Serde.Ser in
            fun t ctx -> (s (option string)) t ctx
          ;;
  
          let _ = serialize_optional
        end [@@ocaml.doc "@inline"] [@@merlin.hide]
      end
  
      module Params = struct
        let id id = DBCaml.Params.Values.integer id
        let _ = id
        let optional optional = DBCaml.Params.Values.text_opt optional
        let _ = optional
      end
  
      module Table = struct
        let drop db = DBCaml.execute db ~params:[] ~query:"DROP TABLE IF EXISTS optional_field"
        let _ = drop
  
        let create db =
          DBCaml.execute
            db
            ~params:[]
            ~query:"CREATE TABLE optional_field (id INTEGER PRIMARY KEY AUTOINCREMENT, optional TEXT ) strict"
        ;;
  
        let _ = create
      end
  
      let relation = "optional_field"
      let _ = relation
  
      let insert ?optional db =
        match
          DBCaml.query
            db
            ~params:[ Params.optional optional ]
            ~query:"INSERT INTO optional_field (optional) VALUES (?) RETURNING *"
            ~deserializer:deserialize_row
        with
        | Ok (t :: []) -> Ok t
        | Ok [] -> Error (`msg "empty: Should have returned one item")
        | Ok _ -> Error (`msg "empty: Should not return more than one item")
        | Error err -> Error err
      ;;
  
      let _ = insert
      let () = Octane.TableRegistry.register { name = "optional_field"; fields = [] }
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  end
< language: ocaml
