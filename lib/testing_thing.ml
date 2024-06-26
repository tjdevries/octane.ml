let get_x () =
  let x =
    object
      method foo = 1
    end
  in
  x
;;

let _ =
  let x = get_x () in
  x#foo
;;
