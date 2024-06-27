Pretty print the file

  $ pp_query ./lib/table.ml | ocamlformat --impl -
  let _ = Format.printf "Hello, world!\n%!"
  
  module User = struct
    type t =
      { id : int
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
            let* id =
              Stdlib.Option.to_result
                ~none:(`Msg "missing field \"id\" (\"id\")")
                !id
            in
            let* name =
              Stdlib.Option.to_result
                ~none:(`Msg "missing field \"name\" (\"name\")")
                !name
            in
            let* age =
              Stdlib.Option.to_result
                ~none:(`Msg "missing field \"age\" (\"age\")")
                !age
            in
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
      let relation = "users"
      let _ = relation
  
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
            let* id =
              Stdlib.Option.to_result
                ~none:(`Msg "missing field \"id\" (\"id\")")
                !id
            in
            let* name =
              Stdlib.Option.to_result
                ~none:(`Msg "missing field \"name\" (\"name\")")
                !name
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
  
    let query db =
      let query =
        Stdlib.Format.sprintf
          "SELECT %s FROM %s"
          (Stdlib.String.concat
             ", "
             [ Stdlib.Format.sprintf "%s.%s" User.relation "id"
             ; Stdlib.Format.sprintf "%s.%s" User.relation "name"
             ])
          User.relation
      in
      Fmt.epr "query: %s@." query;
      Silo_postgres.query db ~query ~deserializer:deserialize
    ;;
  
    let raw = "select User.id, User.name from User"
  end [@warning "-32"]
< language: ocaml
