open Core

module User = struct
  type t =
    { id : int
    ; name : string
    ; phone_number : string
    }
  [@@deriving table { name = "users" }]
end

let () = Stdlib.print_endline "yayayaya"

let _ =
  let tables = !Octane.TableRegistry.registry in
  List.iter tables ~f:(Fmt.pr "%a@." Octane.TableRegistry.pp_table)
;;
