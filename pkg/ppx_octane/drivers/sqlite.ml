module T : Driver.DATABASE = struct
  let name = "sqlite"

  let create_table ~name ~columns =
    [%string "CREATE TABLE %{name} (%{columns}) strict"]
  ;;

  let drop_table ~name = [%string "DROP TABLE IF EXISTS %{name}"]
end

include T
