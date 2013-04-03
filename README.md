Lethink
=======

Use
---

```
> lethink:start().
> lethink:add_pool(my_db_pool, "localhost", 28015, 5, []).
> lethink:db_create(my_db_pool, "superheroes").
> lethink:db_list(my_db_pool).
> lethink:db_drop(my_db_pool, "superheroes").
```
