module type DATABASE = sig
  val name : string
  val create_table : name:string -> columns:string -> string
  val drop_table : name:string -> string
  val coretype_to_sql : string -> string option
end
