Pretty print the file

  $ pp_query ./lib/table.ml > ./lib/table-generated.ml
  $ ocamlformat ./lib/table-generated.ml
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
  
      module Params = struct
        let id id = Dbcaml.Params.Number id
        let _ = id
        let name name = Dbcaml.Params.String name
        let _ = name
        let age age = Dbcaml.Params.Number age
        let _ = age
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
          (String.concat ~sep:", " [ User.relation ])
      in
      let params = [] in
      Fmt.epr "query: %s@." query;
      Silo_postgres.query db ~query ~params ~deserializer:deserialize
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
            let* id =
              Stdlib.Option.to_result
                ~none:(`Msg "missing field \"id\" (\"id\")")
                !id
            in
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
      let relation = "users"
      let _ = relation
  
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
        let id id = Dbcaml.Params.Number id
        let _ = id
      end
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
  
    let query db ~id =
      let query =
        Stdlib.Format.sprintf
          "SELECT %s FROM %s WHERE %s"
          (Stdlib.String.concat
             ", "
             [ Stdlib.Format.sprintf "%s.%s" User.relation "id"
             ; Stdlib.Format.sprintf "%s.%s" User.relation "name"
             ])
          (String.concat ~sep:", " [ User.relation ])
          (Stdlib.Format.sprintf
             "(%s = %s)"
             (Stdlib.Format.sprintf "%s.%s" User.relation "id")
             "$1")
      in
      let params = [ User.Params.id id ] in
      Fmt.epr "query: %s@." query;
      Silo_postgres.query db ~query ~params ~deserializer:deserialize
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
      let relation = "users"
      let _ = relation
  
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
        let id id = Dbcaml.Params.Number id
        let _ = id
        let name name = Dbcaml.Params.String name
        let _ = name
      end
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
            let* name =
              Stdlib.Option.to_result
                ~none:(`Msg "missing field \"name\" (\"name\")")
                !name
            in
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
          (Stdlib.String.concat
             ", "
             [ Stdlib.Format.sprintf "%s.%s" User.relation "name"; p2 ])
          (String.concat ~sep:", " [ User.relation ])
          "TODO"
      in
      let params = [ p1; p2 ] in
      Fmt.epr "query: %s@." query;
      Silo_postgres.query db ~query ~params ~deserializer:deserialize
    ;;
  
    let raw = "SELECT User.name, $2 FROM User WHERE User.id = $1"
  end [@warning "-32"]
< language: ocaml

  $ pp_query ./lib/foreign.ml | ocamlformat --impl -
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
      let relation = "users"
      let _ = relation
  
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
        let id id = Dbcaml.Params.Number id
        let _ = id
        let name name = Dbcaml.Params.String name
        let _ = name
      end
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  end
  
  module Post = struct
    type t =
      { id : int
      ; author : User.Fields.id
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
                | "author" -> Ok `author
                | "id" -> Ok `id
                | _ -> Ok `invalid_tag
              in
              let visit_int _ctx str =
                match str with
                | 0 -> Ok `content
                | 1 -> Ok `author
                | 2 -> Ok `id
                | _ -> Ok `invalid_tag
              in
              Visitor.make ~visit_string ~visit_int ()
            in
            let id = ref None in
            let author = ref None in
            let content = ref None in
            let rec read_fields () =
              let* tag = next_field ctx field_visitor in
              match tag with
              | Some `content ->
                let* v = field ctx "content" string in
                content := Some v;
                read_fields ()
              | Some `author ->
                let* v = field ctx "author" (d User.Fields.deserialize_id) in
                author := Some v;
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
            let* author =
              Stdlib.Option.to_result
                ~none:(`Msg "missing field \"author\" (\"author\")")
                !author
            in
            let* content =
              Stdlib.Option.to_result
                ~none:(`Msg "missing field \"content\" (\"content\")")
                !content
            in
            Ok { content; author; id })
      ;;
  
      let _ = deserialize_t
  
      let serialize_t =
        let ( let* ) = Stdlib.Result.bind in
        let _ = ( let* ) in
        let open Serde.Ser in
        fun t ctx ->
          record ctx "t" 3 (fun ctx ->
            let* () = field ctx "id" (int t.id) in
            let* () =
              field ctx "author" ((s User.Fields.serialize_id) t.author)
            in
            let* () = field ctx "content" (string t.content) in
            Ok ())
      ;;
  
      let _ = serialize_t
      let relation = "posts"
      let _ = relation
  
      module Fields = struct
        let id = "id"
        let _ = id
        let author = "author"
        let _ = author
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
  
        type author = User.Fields.id [@@deriving deserialize, serialize]
  
        include struct
          let _ = fun (_ : author) -> ()
  
          open! Serde
  
          let deserialize_author =
            let ( let* ) = Stdlib.Result.bind in
            let _ = ( let* ) in
            let open Serde.De in
            fun ctx -> (d User.Fields.deserialize_id) ctx
          ;;
  
          let _ = deserialize_author
  
          let serialize_author =
            let ( let* ) = Stdlib.Result.bind in
            let _ = ( let* ) in
            let open Serde.Ser in
            fun t ctx -> (s User.Fields.serialize_id) t ctx
          ;;
  
          let _ = serialize_author
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
        let id id = Dbcaml.Params.Number id
        let _ = id
        let author author = User.Params.id author
        let _ = author
        let content content = Dbcaml.Params.String content
        let _ = content
      end
    end [@@ocaml.doc "@inline"] [@@merlin.hide]
  end
< language: ocaml

  $ pp_query ./lib/invalid_model.ml | ocamlformat --impl -
  module ShouldError = struct
    type t =
      [%ocaml.error "Invalid Model: Module 'Post' is not selected in query"]
  
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
            let* () =
              field ctx "content" ((s Post.Fields.serialize_content) t.content)
            in
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
                let* v =
                  field ctx "content" (d Post.Fields.deserialize_content)
                in
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
            let* name =
              Stdlib.Option.to_result
                ~none:(`Msg "missing field \"name\" (\"name\")")
                !name
            in
            let* content =
              Stdlib.Option.to_result
                ~none:(`Msg "missing field \"content\" (\"content\")")
                !content
            in
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
             [ Stdlib.Format.sprintf "%s.%s" User.relation "name"
             ; Stdlib.Format.sprintf "%s.%s" Post.relation "content"
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
                       (Stdlib.Format.sprintf "%s.%s" User.relation "id")
                       (Stdlib.Format.sprintf "%s.%s" Post.relation "author"))
                ]))
      in
      let params = [] in
      Fmt.epr "query: %s@." query;
      Silo_postgres.query db ~query ~params ~deserializer:deserialize
    ;;
  
    let raw =
      " SELECT User.name, Post.content FROM Post INNER JOIN User ON User.id = \
       Post.author "
    ;;
  end [@warning "-32"]
< language: ocaml

