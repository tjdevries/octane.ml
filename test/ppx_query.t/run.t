Pretty print the file

  $ pp_query ./lib/table.ml | ocamlformat --impl -
  Fatal error: exception Failure("TypedColumn")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Base__List.count_map in file "src/list.ml", line 482, characters 13-17
  Called from Base__List.map in file "src/list.ml" (inlined), line 510, characters 15-31
  Called from Ppx_query__Gen.of_expressions in file "ppx_query/gen.ml", line 120, characters 14-58
  Called from Ppx_query__Gen.to_select_string in file "ppx_query/gen.ml", line 100, characters 20-51
  Called from Ppx_query__Gen.of_ast in file "ppx_query/gen.ml", line 61, characters 21-49
  Called from Ppx_query.letter_rule.(fun) in file "ppx_query/ppx_query.ml", line 123, characters 23-42
  Called from Ppxlib__Ast_pattern_generated.pconst_string.(fun) in file "src/ast_pattern_generated.ml", line 612, characters 27-42
  Called from Ppxlib__Ast_pattern.(^::).(fun) in file "src/ast_pattern.ml", line 110, characters 18-33
  Called from Ppxlib__Ast_pattern.(^::).(fun) in file "src/ast_pattern.ml", line 110, characters 18-33
  Called from Ppxlib__Ast_pattern.map_result.(fun) in file "src/ast_pattern.ml", line 170, characters 53-71
  Called from Ppxlib__Ast_pattern.parse_res in file "src/ast_pattern.ml", line 9, characters 9-36
  Called from Ppxlib__Extension.For_context.convert_inline_res.(fun) in file "src/extension.ml", line 274, characters 8-66
  Called from Ppxlib__Context_free.map_top_down#structure.loop in file "src/context_free.ml", line 675, characters 16-73
  Called from Ppxlib__Context_free.map_top_down#structure.loop.(fun) in file "src/context_free.ml", line 732, characters 20-49
  Called from Ppxlib__Common.With_errors.(>>=) in file "src/common.ml", line 266, characters 21-24
  Called from Ppxlib__Driver.Transform.merge_into_generic_mappers.map_impl in file "src/driver.ml", line 280, characters 6-73
  Called from Ppxlib__Driver.apply_transforms.(fun) in file "src/driver.ml", line 567, characters 19-29
  Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
  Called from Ppxlib__Driver.apply_transforms in file "src/driver.ml", line 543, characters 4-1023
  Called from Ppxlib__Driver.map_structure_gen in file "src/driver.ml", line 692, characters 4-273
  Called from Ppxlib__Driver.process_ast in file "src/driver.ml", line 1054, characters 10-127
  Called from Ppxlib__Driver.process_file in file "src/driver.ml", line 1099, characters 15-111
  Called from Ppxlib__Driver.standalone in file "src/driver.ml", line 1522, characters 9-27
  Re-raised at Location.report_exception.loop in file "parsing/location.ml", line 1013, characters 14-25
  Called from Ppxlib__Driver.standalone in file "src/driver.ml", line 1525, characters 4-61
  Called from Dune__exe__Pp_query in file "bin/pp_query.ml", line 1, characters 9-36
< language: ocaml

  $ pp_query ./lib/where_id.ml | ocamlformat --impl -
  Fatal error: exception Failure("TypedColumn")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Base__List.count_map in file "src/list.ml", line 482, characters 13-17
  Called from Base__List.map in file "src/list.ml" (inlined), line 510, characters 15-31
  Called from Ppx_query__Gen.of_expressions in file "ppx_query/gen.ml", line 120, characters 14-58
  Called from Ppx_query__Gen.to_select_string in file "ppx_query/gen.ml", line 100, characters 20-51
  Called from Ppx_query__Gen.of_ast in file "ppx_query/gen.ml", line 61, characters 21-49
  Called from Ppx_query.letter_rule.(fun) in file "ppx_query/ppx_query.ml", line 123, characters 23-42
  Called from Ppxlib__Ast_pattern_generated.pconst_string.(fun) in file "src/ast_pattern_generated.ml", line 612, characters 27-42
  Called from Ppxlib__Ast_pattern.(^::).(fun) in file "src/ast_pattern.ml", line 110, characters 18-33
  Called from Ppxlib__Ast_pattern.(^::).(fun) in file "src/ast_pattern.ml", line 110, characters 18-33
  Called from Ppxlib__Ast_pattern.map_result.(fun) in file "src/ast_pattern.ml", line 170, characters 53-71
  Called from Ppxlib__Ast_pattern.parse_res in file "src/ast_pattern.ml", line 9, characters 9-36
  Called from Ppxlib__Extension.For_context.convert_inline_res.(fun) in file "src/extension.ml", line 274, characters 8-66
  Called from Ppxlib__Context_free.map_top_down#structure.loop in file "src/context_free.ml", line 675, characters 16-73
  Called from Ppxlib__Context_free.map_top_down#structure.loop.(fun) in file "src/context_free.ml", line 732, characters 20-49
  Called from Ppxlib__Common.With_errors.(>>=) in file "src/common.ml", line 266, characters 21-24
  Called from Ppxlib__Driver.Transform.merge_into_generic_mappers.map_impl in file "src/driver.ml", line 280, characters 6-73
  Called from Ppxlib__Driver.apply_transforms.(fun) in file "src/driver.ml", line 567, characters 19-29
  Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
  Called from Ppxlib__Driver.apply_transforms in file "src/driver.ml", line 543, characters 4-1023
  Called from Ppxlib__Driver.map_structure_gen in file "src/driver.ml", line 692, characters 4-273
  Called from Ppxlib__Driver.process_ast in file "src/driver.ml", line 1054, characters 10-127
  Called from Ppxlib__Driver.process_file in file "src/driver.ml", line 1099, characters 15-111
  Called from Ppxlib__Driver.standalone in file "src/driver.ml", line 1522, characters 9-27
  Re-raised at Location.report_exception.loop in file "parsing/location.ml", line 1013, characters 14-25
  Called from Ppxlib__Driver.standalone in file "src/driver.ml", line 1525, characters 4-61
  Called from Dune__exe__Pp_query in file "bin/pp_query.ml", line 1, characters 9-36
< language: ocaml

  $ pp_query ./lib/where_positional.ml | ocamlformat --impl -
  Fatal error: exception Failure("TypedColumn")
  Raised at Stdlib.failwith in file "stdlib.ml", line 29, characters 17-33
  Called from Base__List.count_map in file "src/list.ml", line 482, characters 13-17
  Called from Base__List.map in file "src/list.ml" (inlined), line 510, characters 15-31
  Called from Ppx_query__Gen.of_expressions in file "ppx_query/gen.ml", line 120, characters 14-58
  Called from Ppx_query__Gen.to_select_string in file "ppx_query/gen.ml", line 100, characters 20-51
  Called from Ppx_query__Gen.of_ast in file "ppx_query/gen.ml", line 61, characters 21-49
  Called from Ppx_query.letter_rule.(fun) in file "ppx_query/ppx_query.ml", line 123, characters 23-42
  Called from Ppxlib__Ast_pattern_generated.pconst_string.(fun) in file "src/ast_pattern_generated.ml", line 612, characters 27-42
  Called from Ppxlib__Ast_pattern.(^::).(fun) in file "src/ast_pattern.ml", line 110, characters 18-33
  Called from Ppxlib__Ast_pattern.(^::).(fun) in file "src/ast_pattern.ml", line 110, characters 18-33
  Called from Ppxlib__Ast_pattern.map_result.(fun) in file "src/ast_pattern.ml", line 170, characters 53-71
  Called from Ppxlib__Ast_pattern.parse_res in file "src/ast_pattern.ml", line 9, characters 9-36
  Called from Ppxlib__Extension.For_context.convert_inline_res.(fun) in file "src/extension.ml", line 274, characters 8-66
  Called from Ppxlib__Context_free.map_top_down#structure.loop in file "src/context_free.ml", line 675, characters 16-73
  Called from Ppxlib__Context_free.map_top_down#structure.loop.(fun) in file "src/context_free.ml", line 732, characters 20-49
  Called from Ppxlib__Common.With_errors.(>>=) in file "src/common.ml", line 266, characters 21-24
  Called from Ppxlib__Driver.Transform.merge_into_generic_mappers.map_impl in file "src/driver.ml", line 280, characters 6-73
  Called from Ppxlib__Driver.apply_transforms.(fun) in file "src/driver.ml", line 567, characters 19-29
  Called from Stdlib__List.fold_left in file "list.ml", line 123, characters 24-34
  Called from Ppxlib__Driver.apply_transforms in file "src/driver.ml", line 543, characters 4-1023
  Called from Ppxlib__Driver.map_structure_gen in file "src/driver.ml", line 692, characters 4-273
  Called from Ppxlib__Driver.process_ast in file "src/driver.ml", line 1054, characters 10-127
  Called from Ppxlib__Driver.process_file in file "src/driver.ml", line 1099, characters 15-111
  Called from Ppxlib__Driver.standalone in file "src/driver.ml", line 1522, characters 9-27
  Re-raised at Location.report_exception.loop in file "parsing/location.ml", line 1013, characters 14-25
  Called from Ppxlib__Driver.standalone in file "src/driver.ml", line 1525, characters 4-61
  Called from Dune__exe__Pp_query in file "bin/pp_query.ml", line 1, characters 9-36
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

