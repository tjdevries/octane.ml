(* module GetUserByID = [%query select User.name, User.age from User where User.id = $id] *)

let parsed = Parser.query
