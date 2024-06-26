
Problem: I'm not satisfied with how we write sql in ocaml my caml
- can we do ecto but way cooler? and better (because ocaml?)

Options I wanted to Explore:

### Option 1:

```ocaml

module Users = struct (* table declaration here *) end

let%sql result = select * from Users where id = 1 in
assert false

let result = [%sql select * from Users where id = 1] in
assert false

```

### Option 2:

what if we had a special filetype?... `oql` ?

```ocaml

(* query.oql *) 
let query = select * from users where id = 1

(* this turns into an ocaml file that looks like *)
let query = Query.select ...

```

### Option 3: Printf method

Somehow (i don't know how this works yet...)

Fmt.pr or Format.printf take a string and turn it into a magic gadt...

so

```ocaml
(* If you don't pass str and str here, it will not compile... HOW DO??? *)
let s = Format.sprintf "Hello %s world %s" str str

(* sql version would just be *)
let x = Query.exec "select * from users where id = 1"
```

### Option 4: Ecto style

```elixir
x = Repo.all(from u in User, where: u.id == 1)
```

```ocaml
x = Expr.(select * from Users where Users.id = 1)
x = Expr.select ~from:Users ~where:(Users.id = 1)
```

### Option 5: Module style

```ocaml
(* You write... *)
module GetUser = [%query select * from Users where id = $id]
```

```ocaml

module Users : TABLE = struct
    type t = { id: string; name: string }
    [@@deriving table]
end


module Posts = [%table { id: string; user_id: string; title: string }]
module Posts = struct
    type t = { id: string; user_id: string; title: string }
    [@@deriving table]

    module Fields = struct
        type id = string
        type user_id = string
        type title = string
    end

    let table_name = "posts"
end

(* you write this *)
module Users = MyApp.Models.Users
module NameAndTitle = [%query
    SELECT Users.name, Posts.title
        FROM Users JOIN Posts ON Users.id = Posts.user_id
        WHERE Users.id = $id
    ]


(* you get this *)
module NameAndTitle = struct
    type t = { name: Users.Fields.name; title: string }

    let t = 
      object
        method name = Users.name response
     end

    let get_one db ~id = assert false
    (* let get_many db ... *)
end

let res : (NameAndTitle.t, errors) result = NameAndTitle.get_one db ~id:1

```




# Problems


```ocaml
(* What can the type of x be?? *)
let x = Query.exec "select user.name, post.title from Users join Posts on user.id = post.user_id"
```

Options:
- `string * string`
- CANT: `{ name: string; title: string }`, because there is nowhere to save the type
- `< name: string; title: string >`
- `struct type t = { name: string; title: string } ... end`
    - Is it expensive to make a module that does this? I'm not sure...





I want to write a struct that represents a table
- i want migrations
- i want updating migrations
- i want my queries to update
- i want my CODE to be the source of truth, not whatever state my local db is in
(no users btw)






