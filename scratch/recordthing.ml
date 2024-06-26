let get_x () =
  let open struct
    type tmp = { x : int }
  end in
  let x = { x = 1 } in
  x
;;
