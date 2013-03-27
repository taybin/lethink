-ifndef(VERSIONDUMMY_PB_H).
-define(VERSIONDUMMY_PB_H, true).
-record(versiondummy, {
    
}).
-endif.

-ifndef(QUERY_PB_H).
-define(QUERY_PB_H, true).
-record(query, {
    type,
    query,
    token,
    noreply = false,
    global_optargs = []
}).
-endif.

-ifndef(FRAME_PB_H).
-define(FRAME_PB_H, true).
-record(frame, {
    type,
    pos,
    opt
}).
-endif.

-ifndef(BACKTRACE_PB_H).
-define(BACKTRACE_PB_H, true).
-record(backtrace, {
    frames = []
}).
-endif.

-ifndef(RESPONSE_PB_H).
-define(RESPONSE_PB_H, true).
-record(response, {
    type,
    token,
    response = [],
    backtrace
}).
-endif.

-ifndef(DATUM_PB_H).
-define(DATUM_PB_H, true).
-record(datum, {
    type,
    r_bool,
    r_num,
    r_str,
    r_array = [],
    r_object = [],
    '$extensions' = dict:new()
}).
-endif.

-ifndef(TERM_PB_H).
-define(TERM_PB_H, true).
-record(term, {
    type,
    datum,
    args = [],
    optargs = [],
    '$extensions' = dict:new()
}).
-endif.

-ifndef(QUERY_ASSOCPAIR_PB_H).
-define(QUERY_ASSOCPAIR_PB_H, true).
-record(query_assocpair, {
    key,
    val
}).
-endif.

-ifndef(DATUM_ASSOCPAIR_PB_H).
-define(DATUM_ASSOCPAIR_PB_H, true).
-record(datum_assocpair, {
    key,
    val
}).
-endif.

-ifndef(TERM_ASSOCPAIR_PB_H).
-define(TERM_ASSOCPAIR_PB_H, true).
-record(term_assocpair, {
    key,
    val
}).
-endif.

