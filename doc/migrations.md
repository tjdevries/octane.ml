Step 1: Find out what state of database should be
    - (discussed below)

Step 2: Find out what state of database was
    - You save this in some file somewhere or it's built via iterating over the migrations

Step 3: Generate diff
    - just algebra (famous last words again)

Step 4: Apply diff (as migration)
    - just algebra, part 2 (famous lastest words, part 3)


# How to find out what the state of the database *should* be

Option 1:
- parse the entire tree, and then build the migrations from the syntax tree
    - there already exists libraries for Parsetree in OCaml, so we can re-use that.
    - And i can filter based on which ones have the `%table` ppx used, which is not so hard (famous last words)

Option 2:
- use the compiled artifacts to generate migrations. These have type information, which is useful

Option 3:
- inside of our macro/ppx, can we generate the information we'd need to be able to generate migrations?

Option 4:
- we have a separate implementation of the ppx that runs only when doing `octane make-migrations` or whatever.
    - Since we just need a module that has the same interface, we could do something different depending on compilation flags

# Now that we found it, we need to generate a migration

All of the above options, after collecting the state of the current expected tables, you would diff that against
the previous version of the tables and then generate a migration that would reconcile the two (if possible).
- In general, this would be inside of a transaction, so if it fails, then it all gets rolled back.


# Open Question

You have project Foo
It has dep Bar

dep Bar declares some tables (maybe it's something like Oban for ocaml now)
... can we find out that this has happened?
... what do other migration tools do?


