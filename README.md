Lethink
=======

An erlang driver for [rethinkdb](http://rethinkdb.com).

Status
------
[![Build Status](https://travis-ci.org/taybin/lethink.png?branch=master)](https://travis-ci.org/taybin/lethink)

Use
---

```
> lethink:start().
> application:set_env(lethink, timeout, 60000).
> lethink:add_pool(my_db_pool, 5, [{database, "localhost"}, {port, 28015}]).
> lethink:query(my_db_pool, [{db_create, <<"superheroes">>}]).
> lethink:query(my_db_pool, [{db_list}]).
> lethink:query(my_db_pool, [{db_drop, <<"superheroes">>}]).
> lethink:query(my_db_pool, [{db, <<"superheroes">>}, {table_create, <<"marvel">>}]).
> lethink:query(my_db_pool, [{db, <<"superheroes">>},
                             {table_create, <<"marvel">>, [{primary_key, <<"name">>}]}]).
> lethink:query(my_db_pool, [{db, <<"superheroes">>}, {table_list}]).
> lethink:query(my_db_pool, [{db, <<"superheroes">>}, {table_drop, <<"marvel">>}]).
> lethink:use(my_db_pool, <<"superheroes">>).
> JsonProplist = [{[{<<"id">>, 5}, {<<"name">>, <<"batman">>}, {<<"rich">>, true}, {<<"cars">>, [1,2,3]}]},
                  {[{<<"id">>, 6}, {<<"name">>, <<"robin">>}, {<<"rich">>, false}, {<<"cars">>, null}]}].
> lethink:use(my_db_pool, [{table, <<"marvel">>},
                           {insert, JsonProplist}].
> lethink:use(my_db_pool, [{table, <<"marvel">>}, {get, 5}]).
```
