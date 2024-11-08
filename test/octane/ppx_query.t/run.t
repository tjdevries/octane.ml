Pretty print the file

  $ pp_query ./lib/table.ml > ./lib/table-generated.ml
  File "./lib/table.ml", line 6, characters 6-33:
  6 |     ; middle_name : string option
            ^^^^^^^^^^^^^^^^^^^^^^^^^^^
  Error: Unknown type: coretype_to_create_field
  [1]
  $ ocamlformat ./lib/table-generated.ml
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
  File "./lib/foreign.ml", lines 12-13, characters 6-5:
  12 | ......author : User.Fields.id
  13 |     ;.................
  Error: create_field - ldot
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
