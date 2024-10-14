let print_name obj = 
  print_endline obj#name

let print_name_and_age obj =
  print_endline obj#name;
  print_int obj#age
;;
