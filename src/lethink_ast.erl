-module(lethink_ast).

-export([build_query/2]).

-include("ql2_pb.hrl").

build_query({db, Name}, []) ->
    #term {
        type = 'DB',
        args = ql2_util:datum_term(Name)
    };

build_query({table, Name}, []) ->
    build_query({table, Name, false}, []);

build_query({table, Name, UseOutdated}, []) ->
    #term {
        type = 'TABLE',
        args = ql2_util:datum_term(Name),
        optargs = [ql2_util:term_assocpair(<<"use_outdated">>, UseOutdated)]
    };

build_query({table, Name}, #term{ type = 'DB' } = Db) ->
    build_query({table, Name, false}, Db);

build_query({table, Name, UseOutdated}, #term{ type = 'DB' } = Db) ->
    #term {
        type = 'TABLE',
        args = [Db] ++ [ql2_util:datum_term(Name)],
        optargs = [ql2_util:term_assocpair(<<"use_outdated">>, UseOutdated)]
    };

build_query({insert, Data}, #term{ type = 'TABLE' } = Table) ->
    #term {
        type = 'INSERT',
        args = [Table] ++ [ql2_util:datum_term(Data)]
    };

build_query({get, Key}, #term{ type = 'TABLE' } = Table) ->
    #term {
        type = 'GET',
        args = [Table] ++ [ql2_util:datum_term(Key)]
    }.
