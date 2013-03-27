-file("src/ql2_pb.erl", 1).

-module(ql2_pb).

-export([encode_term_assocpair/1,
	 decode_term_assocpair/1, encode_datum_assocpair/1,
	 decode_datum_assocpair/1, encode_query_assocpair/1,
	 decode_query_assocpair/1, encode_term/1, decode_term/1,
	 encode_datum/1, decode_datum/1, encode_response/1,
	 decode_response/1, encode_backtrace/1,
	 decode_backtrace/1, encode_frame/1, decode_frame/1,
	 encode_query/1, decode_query/1, encode_versiondummy/1,
	 decode_versiondummy/1]).

-export([has_extension/2, extension_size/1,
	 get_extension/2, set_extension/3]).

-export([decode_extensions/1]).

-export([encode/1, decode/2]).

-record(term_assocpair, {key, val}).

-record(datum_assocpair, {key, val}).

-record(query_assocpair, {key, val}).

-record(term,
	{type, datum, args, optargs, '$extensions'}).

-record(datum,
	{type, r_bool, r_num, r_str, r_array, r_object,
	 '$extensions'}).

-record(response, {type, token, response, backtrace}).

-record(backtrace, {frames}).

-record(frame, {type, pos, opt}).

-record(query,
	{type, query, token, noreply, global_optargs}).

-record(versiondummy, {}).

encode(Record) -> encode(element(1, Record), Record).

encode_term_assocpair(Record)
    when is_record(Record, term_assocpair) ->
    encode(term_assocpair, Record).

encode_datum_assocpair(Record)
    when is_record(Record, datum_assocpair) ->
    encode(datum_assocpair, Record).

encode_query_assocpair(Record)
    when is_record(Record, query_assocpair) ->
    encode(query_assocpair, Record).

encode_term(Record) when is_record(Record, term) ->
    encode(term, Record).

encode_datum(Record) when is_record(Record, datum) ->
    encode(datum, Record).

encode_response(Record)
    when is_record(Record, response) ->
    encode(response, Record).

encode_backtrace(Record)
    when is_record(Record, backtrace) ->
    encode(backtrace, Record).

encode_frame(Record) when is_record(Record, frame) ->
    encode(frame, Record).

encode_query(Record) when is_record(Record, query) ->
    encode(query, Record).

encode_versiondummy(Record)
    when is_record(Record, versiondummy) ->
    encode(versiondummy, Record).

encode(versiondummy, Record) ->
    [iolist(versiondummy, Record)
     | encode_extensions(Record)];
encode(query, Record) ->
    [iolist(query, Record) | encode_extensions(Record)];
encode(frame, Record) ->
    [iolist(frame, Record) | encode_extensions(Record)];
encode(backtrace, Record) ->
    [iolist(backtrace, Record) | encode_extensions(Record)];
encode(response, Record) ->
    [iolist(response, Record) | encode_extensions(Record)];
encode(datum, Record) ->
    [iolist(datum, Record) | encode_extensions(Record)];
encode(term, Record) ->
    [iolist(term, Record) | encode_extensions(Record)];
encode(query_assocpair, Record) ->
    [iolist(query_assocpair, Record)
     | encode_extensions(Record)];
encode(datum_assocpair, Record) ->
    [iolist(datum_assocpair, Record)
     | encode_extensions(Record)];
encode(term_assocpair, Record) ->
    [iolist(term_assocpair, Record)
     | encode_extensions(Record)].

encode_extensions(#datum{'$extensions' = Extends}) ->
    [pack(Key, Optionalness, Data, Type, Accer)
     || {Key, {Optionalness, Data, Type, Accer}}
	    <- dict:to_list(Extends)];
encode_extensions(#term{'$extensions' = Extends}) ->
    [pack(Key, Optionalness, Data, Type, Accer)
     || {Key, {Optionalness, Data, Type, Accer}}
	    <- dict:to_list(Extends)];
encode_extensions(_) -> [].

iolist(versiondummy, _Record) -> [];
iolist(query, Record) ->
    [pack(1, optional,
	  with_default(Record#query.type, none), query_querytype,
	  []),
     pack(2, optional,
	  with_default(Record#query.query, none), term, []),
     pack(3, optional,
	  with_default(Record#query.token, none), int64, []),
     pack(4, optional,
	  with_default(Record#query.noreply, false), bool, []),
     pack(6, repeated,
	  with_default(Record#query.global_optargs, none),
	  query_assocpair, [])];
iolist(frame, Record) ->
    [pack(1, optional,
	  with_default(Record#frame.type, none), frame_frametype,
	  []),
     pack(2, optional, with_default(Record#frame.pos, none),
	  int64, []),
     pack(3, optional, with_default(Record#frame.opt, none),
	  string, [])];
iolist(backtrace, Record) ->
    [pack(1, repeated,
	  with_default(Record#backtrace.frames, none), frame,
	  [])];
iolist(response, Record) ->
    [pack(1, optional,
	  with_default(Record#response.type, none),
	  response_responsetype, []),
     pack(2, optional,
	  with_default(Record#response.token, none), int64, []),
     pack(3, repeated,
	  with_default(Record#response.response, none), datum,
	  []),
     pack(4, optional,
	  with_default(Record#response.backtrace, none),
	  backtrace, [])];
iolist(datum, Record) ->
    [pack(1, optional,
	  with_default(Record#datum.type, none), datum_datumtype,
	  []),
     pack(2, optional,
	  with_default(Record#datum.r_bool, none), bool, []),
     pack(3, optional,
	  with_default(Record#datum.r_num, none), double, []),
     pack(4, optional,
	  with_default(Record#datum.r_str, none), string, []),
     pack(5, repeated,
	  with_default(Record#datum.r_array, none), datum, []),
     pack(6, repeated,
	  with_default(Record#datum.r_object, none),
	  datum_assocpair, [])];
iolist(term, Record) ->
    [pack(1, optional, with_default(Record#term.type, none),
	  term_termtype, []),
     pack(2, optional, with_default(Record#term.datum, none),
	  datum, []),
     pack(3, repeated, with_default(Record#term.args, none),
	  term, []),
     pack(4, repeated,
	  with_default(Record#term.optargs, none), term_assocpair,
	  [])];
iolist(query_assocpair, Record) ->
    [pack(1, optional,
	  with_default(Record#query_assocpair.key, none), string,
	  []),
     pack(2, optional,
	  with_default(Record#query_assocpair.val, none), term,
	  [])];
iolist(datum_assocpair, Record) ->
    [pack(1, optional,
	  with_default(Record#datum_assocpair.key, none), string,
	  []),
     pack(2, optional,
	  with_default(Record#datum_assocpair.val, none), datum,
	  [])];
iolist(term_assocpair, Record) ->
    [pack(1, optional,
	  with_default(Record#term_assocpair.key, none), string,
	  []),
     pack(2, optional,
	  with_default(Record#term_assocpair.val, none), term,
	  [])].

with_default(Default, Default) -> undefined;
with_default(Val, _) -> Val.

pack(_, optional, undefined, _, _) -> [];
pack(_, repeated, undefined, _, _) -> [];
pack(_, repeated_packed, undefined, _, _) -> [];
pack(_, repeated_packed, [], _, _) -> [];
pack(FNum, required, undefined, Type, _) ->
    exit({error,
	  {required_field_is_undefined, FNum, Type}});
pack(_, repeated, [], _, Acc) -> lists:reverse(Acc);
pack(FNum, repeated, [Head | Tail], Type, Acc) ->
    pack(FNum, repeated, Tail, Type,
	 [pack(FNum, optional, Head, Type, []) | Acc]);
pack(FNum, repeated_packed, Data, Type, _) ->
    protobuffs:encode_packed(FNum, Data, Type);
pack(FNum, _, Data, _, _) when is_tuple(Data) ->
    [RecName | _] = tuple_to_list(Data),
    protobuffs:encode(FNum, encode(RecName, Data), bytes);
pack(FNum, _, Data, Type, _)
    when Type =:= bool;
	 Type =:= int32;
	 Type =:= uint32;
	 Type =:= int64;
	 Type =:= uint64;
	 Type =:= sint32;
	 Type =:= sint64;
	 Type =:= fixed32;
	 Type =:= sfixed32;
	 Type =:= fixed64;
	 Type =:= sfixed64;
	 Type =:= string;
	 Type =:= bytes;
	 Type =:= float;
	 Type =:= double ->
    protobuffs:encode(FNum, Data, Type);
pack(FNum, _, Data, Type, _) when is_atom(Data) ->
    protobuffs:encode(FNum, enum_to_int(Type, Data), enum).

enum_to_int(term_termtype, 'DESC') -> 74;
enum_to_int(term_termtype, 'ASC') -> 73;
enum_to_int(term_termtype, 'FUNC') -> 69;
enum_to_int(term_termtype, 'FOREACH') -> 68;
enum_to_int(term_termtype, 'ALL') -> 67;
enum_to_int(term_termtype, 'ANY') -> 66;
enum_to_int(term_termtype, 'BRANCH') -> 65;
enum_to_int(term_termtype, 'FUNCALL') -> 64;
enum_to_int(term_termtype, 'TABLE_LIST') -> 62;
enum_to_int(term_termtype, 'TABLE_DROP') -> 61;
enum_to_int(term_termtype, 'TABLE_CREATE') -> 60;
enum_to_int(term_termtype, 'DB_LIST') -> 59;
enum_to_int(term_termtype, 'DB_DROP') -> 58;
enum_to_int(term_termtype, 'DB_CREATE') -> 57;
enum_to_int(term_termtype, 'INSERT') -> 56;
enum_to_int(term_termtype, 'REPLACE') -> 55;
enum_to_int(term_termtype, 'DELETE') -> 54;
enum_to_int(term_termtype, 'UPDATE') -> 53;
enum_to_int(term_termtype, 'TYPEOF') -> 52;
enum_to_int(term_termtype, 'COERCE_TO') -> 51;
enum_to_int(term_termtype, 'ZIP') -> 72;
enum_to_int(term_termtype, 'EQ_JOIN') -> 50;
enum_to_int(term_termtype, 'OUTER_JOIN') -> 49;
enum_to_int(term_termtype, 'INNER_JOIN') -> 48;
enum_to_int(term_termtype, 'GROUPBY') -> 47;
enum_to_int(term_termtype, 'GROUPED_MAP_REDUCE') -> 46;
enum_to_int(term_termtype, 'NTH') -> 45;
enum_to_int(term_termtype, 'UNION') -> 44;
enum_to_int(term_termtype, 'COUNT') -> 43;
enum_to_int(term_termtype, 'DISTINCT') -> 42;
enum_to_int(term_termtype, 'ORDERBY') -> 41;
enum_to_int(term_termtype, 'CONCATMAP') -> 40;
enum_to_int(term_termtype, 'FILTER') -> 39;
enum_to_int(term_termtype, 'MAP') -> 38;
enum_to_int(term_termtype, 'REDUCE') -> 37;
enum_to_int(term_termtype, 'BETWEEN') -> 36;
enum_to_int(term_termtype, 'MERGE') -> 35;
enum_to_int(term_termtype, 'WITHOUT') -> 34;
enum_to_int(term_termtype, 'PLUCK') -> 33;
enum_to_int(term_termtype, 'CONTAINS') -> 32;
enum_to_int(term_termtype, 'GETATTR') -> 31;
enum_to_int(term_termtype, 'LIMIT') -> 71;
enum_to_int(term_termtype, 'SKIP') -> 70;
enum_to_int(term_termtype, 'SLICE') -> 30;
enum_to_int(term_termtype, 'APPEND') -> 29;
enum_to_int(term_termtype, 'MOD') -> 28;
enum_to_int(term_termtype, 'DIV') -> 27;
enum_to_int(term_termtype, 'MUL') -> 26;
enum_to_int(term_termtype, 'SUB') -> 25;
enum_to_int(term_termtype, 'ADD') -> 24;
enum_to_int(term_termtype, 'NOT') -> 23;
enum_to_int(term_termtype, 'GE') -> 22;
enum_to_int(term_termtype, 'GT') -> 21;
enum_to_int(term_termtype, 'LE') -> 20;
enum_to_int(term_termtype, 'LT') -> 19;
enum_to_int(term_termtype, 'NE') -> 18;
enum_to_int(term_termtype, 'EQ') -> 17;
enum_to_int(term_termtype, 'GET') -> 16;
enum_to_int(term_termtype, 'TABLE') -> 15;
enum_to_int(term_termtype, 'DB') -> 14;
enum_to_int(term_termtype, 'IMPLICIT_VAR') -> 13;
enum_to_int(term_termtype, 'ERROR') -> 12;
enum_to_int(term_termtype, 'JAVASCRIPT') -> 11;
enum_to_int(term_termtype, 'VAR') -> 10;
enum_to_int(term_termtype, 'MAKE_OBJ') -> 3;
enum_to_int(term_termtype, 'MAKE_ARRAY') -> 2;
enum_to_int(term_termtype, 'DATUM') -> 1;
enum_to_int(datum_datumtype, 'R_OBJECT') -> 6;
enum_to_int(datum_datumtype, 'R_ARRAY') -> 5;
enum_to_int(datum_datumtype, 'R_STR') -> 4;
enum_to_int(datum_datumtype, 'R_NUM') -> 3;
enum_to_int(datum_datumtype, 'R_BOOL') -> 2;
enum_to_int(datum_datumtype, 'R_NULL') -> 1;
enum_to_int(response_responsetype, 'RUNTIME_ERROR') ->
    18;
enum_to_int(response_responsetype, 'COMPILE_ERROR') ->
    17;
enum_to_int(response_responsetype, 'CLIENT_ERROR') ->
    16;
enum_to_int(response_responsetype, 'SUCCESS_PARTIAL') ->
    3;
enum_to_int(response_responsetype,
	    'SUCCESS_SEQUENCE') ->
    2;
enum_to_int(response_responsetype, 'SUCCESS_ATOM') -> 1;
enum_to_int(frame_frametype, 'OPT') -> 2;
enum_to_int(frame_frametype, 'POS') -> 1;
enum_to_int(query_querytype, 'STOP') -> 3;
enum_to_int(query_querytype, 'CONTINUE') -> 2;
enum_to_int(query_querytype, 'START') -> 1;
enum_to_int(versiondummy_version, 'V0_1') -> 1063369270.

int_to_enum(term_termtype, 74) -> 'DESC';
int_to_enum(term_termtype, 73) -> 'ASC';
int_to_enum(term_termtype, 69) -> 'FUNC';
int_to_enum(term_termtype, 68) -> 'FOREACH';
int_to_enum(term_termtype, 67) -> 'ALL';
int_to_enum(term_termtype, 66) -> 'ANY';
int_to_enum(term_termtype, 65) -> 'BRANCH';
int_to_enum(term_termtype, 64) -> 'FUNCALL';
int_to_enum(term_termtype, 62) -> 'TABLE_LIST';
int_to_enum(term_termtype, 61) -> 'TABLE_DROP';
int_to_enum(term_termtype, 60) -> 'TABLE_CREATE';
int_to_enum(term_termtype, 59) -> 'DB_LIST';
int_to_enum(term_termtype, 58) -> 'DB_DROP';
int_to_enum(term_termtype, 57) -> 'DB_CREATE';
int_to_enum(term_termtype, 56) -> 'INSERT';
int_to_enum(term_termtype, 55) -> 'REPLACE';
int_to_enum(term_termtype, 54) -> 'DELETE';
int_to_enum(term_termtype, 53) -> 'UPDATE';
int_to_enum(term_termtype, 52) -> 'TYPEOF';
int_to_enum(term_termtype, 51) -> 'COERCE_TO';
int_to_enum(term_termtype, 72) -> 'ZIP';
int_to_enum(term_termtype, 50) -> 'EQ_JOIN';
int_to_enum(term_termtype, 49) -> 'OUTER_JOIN';
int_to_enum(term_termtype, 48) -> 'INNER_JOIN';
int_to_enum(term_termtype, 47) -> 'GROUPBY';
int_to_enum(term_termtype, 46) -> 'GROUPED_MAP_REDUCE';
int_to_enum(term_termtype, 45) -> 'NTH';
int_to_enum(term_termtype, 44) -> 'UNION';
int_to_enum(term_termtype, 43) -> 'COUNT';
int_to_enum(term_termtype, 42) -> 'DISTINCT';
int_to_enum(term_termtype, 41) -> 'ORDERBY';
int_to_enum(term_termtype, 40) -> 'CONCATMAP';
int_to_enum(term_termtype, 39) -> 'FILTER';
int_to_enum(term_termtype, 38) -> 'MAP';
int_to_enum(term_termtype, 37) -> 'REDUCE';
int_to_enum(term_termtype, 36) -> 'BETWEEN';
int_to_enum(term_termtype, 35) -> 'MERGE';
int_to_enum(term_termtype, 34) -> 'WITHOUT';
int_to_enum(term_termtype, 33) -> 'PLUCK';
int_to_enum(term_termtype, 32) -> 'CONTAINS';
int_to_enum(term_termtype, 31) -> 'GETATTR';
int_to_enum(term_termtype, 71) -> 'LIMIT';
int_to_enum(term_termtype, 70) -> 'SKIP';
int_to_enum(term_termtype, 30) -> 'SLICE';
int_to_enum(term_termtype, 29) -> 'APPEND';
int_to_enum(term_termtype, 28) -> 'MOD';
int_to_enum(term_termtype, 27) -> 'DIV';
int_to_enum(term_termtype, 26) -> 'MUL';
int_to_enum(term_termtype, 25) -> 'SUB';
int_to_enum(term_termtype, 24) -> 'ADD';
int_to_enum(term_termtype, 23) -> 'NOT';
int_to_enum(term_termtype, 22) -> 'GE';
int_to_enum(term_termtype, 21) -> 'GT';
int_to_enum(term_termtype, 20) -> 'LE';
int_to_enum(term_termtype, 19) -> 'LT';
int_to_enum(term_termtype, 18) -> 'NE';
int_to_enum(term_termtype, 17) -> 'EQ';
int_to_enum(term_termtype, 16) -> 'GET';
int_to_enum(term_termtype, 15) -> 'TABLE';
int_to_enum(term_termtype, 14) -> 'DB';
int_to_enum(term_termtype, 13) -> 'IMPLICIT_VAR';
int_to_enum(term_termtype, 12) -> 'ERROR';
int_to_enum(term_termtype, 11) -> 'JAVASCRIPT';
int_to_enum(term_termtype, 10) -> 'VAR';
int_to_enum(term_termtype, 3) -> 'MAKE_OBJ';
int_to_enum(term_termtype, 2) -> 'MAKE_ARRAY';
int_to_enum(term_termtype, 1) -> 'DATUM';
int_to_enum(datum_datumtype, 6) -> 'R_OBJECT';
int_to_enum(datum_datumtype, 5) -> 'R_ARRAY';
int_to_enum(datum_datumtype, 4) -> 'R_STR';
int_to_enum(datum_datumtype, 3) -> 'R_NUM';
int_to_enum(datum_datumtype, 2) -> 'R_BOOL';
int_to_enum(datum_datumtype, 1) -> 'R_NULL';
int_to_enum(response_responsetype, 18) ->
    'RUNTIME_ERROR';
int_to_enum(response_responsetype, 17) ->
    'COMPILE_ERROR';
int_to_enum(response_responsetype, 16) ->
    'CLIENT_ERROR';
int_to_enum(response_responsetype, 3) ->
    'SUCCESS_PARTIAL';
int_to_enum(response_responsetype, 2) ->
    'SUCCESS_SEQUENCE';
int_to_enum(response_responsetype, 1) -> 'SUCCESS_ATOM';
int_to_enum(frame_frametype, 2) -> 'OPT';
int_to_enum(frame_frametype, 1) -> 'POS';
int_to_enum(query_querytype, 3) -> 'STOP';
int_to_enum(query_querytype, 2) -> 'CONTINUE';
int_to_enum(query_querytype, 1) -> 'START';
int_to_enum(versiondummy_version, 1063369270) -> 'V0_1';
int_to_enum(_, Val) -> Val.

decode_term_assocpair(Bytes) when is_binary(Bytes) ->
    decode(term_assocpair, Bytes).

decode_datum_assocpair(Bytes) when is_binary(Bytes) ->
    decode(datum_assocpair, Bytes).

decode_query_assocpair(Bytes) when is_binary(Bytes) ->
    decode(query_assocpair, Bytes).

decode_term(Bytes) when is_binary(Bytes) ->
    decode(term, Bytes).

decode_datum(Bytes) when is_binary(Bytes) ->
    decode(datum, Bytes).

decode_response(Bytes) when is_binary(Bytes) ->
    decode(response, Bytes).

decode_backtrace(Bytes) when is_binary(Bytes) ->
    decode(backtrace, Bytes).

decode_frame(Bytes) when is_binary(Bytes) ->
    decode(frame, Bytes).

decode_query(Bytes) when is_binary(Bytes) ->
    decode(query, Bytes).

decode_versiondummy(Bytes) when is_binary(Bytes) ->
    decode(versiondummy, Bytes).

decode(enummsg_values, 1) -> value1;
decode(versiondummy, Bytes) when is_binary(Bytes) ->
    Types = [],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(versiondummy, Decoded);
decode(query, Bytes) when is_binary(Bytes) ->
    Types = [{6, global_optargs, query_assocpair,
	      [is_record, repeated]},
	     {4, noreply, bool, []}, {3, token, int64, []},
	     {2, query, term, [is_record]},
	     {1, type, query_querytype, []}],
    Defaults = [{4, noreply, false},
		{6, global_optargs, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(query, Decoded);
decode(frame, Bytes) when is_binary(Bytes) ->
    Types = [{3, opt, string, []}, {2, pos, int64, []},
	     {1, type, frame_frametype, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(frame, Decoded);
decode(backtrace, Bytes) when is_binary(Bytes) ->
    Types = [{1, frames, frame, [is_record, repeated]}],
    Defaults = [{1, frames, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(backtrace, Decoded);
decode(response, Bytes) when is_binary(Bytes) ->
    Types = [{4, backtrace, backtrace, [is_record]},
	     {3, response, datum, [is_record, repeated]},
	     {2, token, int64, []},
	     {1, type, response_responsetype, []}],
    Defaults = [{3, response, []}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(response, Decoded);
decode(datum, Bytes) when is_binary(Bytes) ->
    Types = [{6, r_object, datum_assocpair,
	      [is_record, repeated]},
	     {5, r_array, datum, [is_record, repeated]},
	     {4, r_str, string, []}, {3, r_num, double, []},
	     {2, r_bool, bool, []}, {1, type, datum_datumtype, []}],
    Defaults = [{5, r_array, []}, {6, r_object, []},
		{false, '$extensions',
		 {dict, 0, 16, 16, 8, 80, 48,
		  {[], [], [], [], [], [], [], [], [], [], [], [], [], [],
		   [], []},
		  {{[], [], [], [], [], [], [], [], [], [], [], [], [],
		    [], [], []}}}}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(datum, Decoded);
decode(term, Bytes) when is_binary(Bytes) ->
    Types = [{4, optargs, term_assocpair,
	      [is_record, repeated]},
	     {3, args, term, [is_record, repeated]},
	     {2, datum, datum, [is_record]},
	     {1, type, term_termtype, []}],
    Defaults = [{3, args, []}, {4, optargs, []},
		{false, '$extensions',
		 {dict, 0, 16, 16, 8, 80, 48,
		  {[], [], [], [], [], [], [], [], [], [], [], [], [], [],
		   [], []},
		  {{[], [], [], [], [], [], [], [], [], [], [], [], [],
		    [], [], []}}}}],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(term, Decoded);
decode(query_assocpair, Bytes) when is_binary(Bytes) ->
    Types = [{2, val, term, [is_record]},
	     {1, key, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(query_assocpair, Decoded);
decode(datum_assocpair, Bytes) when is_binary(Bytes) ->
    Types = [{2, val, datum, [is_record]},
	     {1, key, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(datum_assocpair, Decoded);
decode(term_assocpair, Bytes) when is_binary(Bytes) ->
    Types = [{2, val, term, [is_record]},
	     {1, key, string, []}],
    Defaults = [],
    Decoded = decode(Bytes, Types, Defaults),
    to_record(term_assocpair, Decoded).

decode(<<>>, _, Acc) -> Acc;
decode(Bytes, Types, Acc) ->
    {ok, FNum} = protobuffs:next_field_num(Bytes),
    case lists:keyfind(FNum, 1, Types) of
      {FNum, Name, Type, Opts} ->
	  {Value1, Rest1} = case lists:member(is_record, Opts) of
			      true ->
				  {{FNum, V}, R} = protobuffs:decode(Bytes,
								     bytes),
				  RecVal = decode(Type, V),
				  {RecVal, R};
			      false ->
				  case lists:member(repeated_packed, Opts) of
				    true ->
					{{FNum, V}, R} =
					    protobuffs:decode_packed(Bytes,
								     Type),
					{V, R};
				    false ->
					{{FNum, V}, R} =
					    protobuffs:decode(Bytes, Type),
					{unpack_value(V, Type), R}
				  end
			    end,
	  case lists:member(repeated, Opts) of
	    true ->
		case lists:keytake(FNum, 1, Acc) of
		  {value, {FNum, Name, List}, Acc1} ->
		      decode(Rest1, Types,
			     [{FNum, Name,
			       lists:reverse([int_to_enum(Type, Value1)
					      | lists:reverse(List)])}
			      | Acc1]);
		  false ->
		      decode(Rest1, Types,
			     [{FNum, Name, [int_to_enum(Type, Value1)]} | Acc])
		end;
	    false ->
		decode(Rest1, Types,
		       [{FNum, Name, int_to_enum(Type, Value1)} | Acc])
	  end;
      false ->
	  case lists:keyfind('$extensions', 2, Acc) of
	    {_, _, Dict} ->
		{{FNum, _V}, R} = protobuffs:decode(Bytes, bytes),
		Diff = size(Bytes) - size(R),
		<<V:Diff/binary, _/binary>> = Bytes,
		NewDict = dict:store(FNum, V, Dict),
		NewAcc = lists:keyreplace('$extensions', 2, Acc,
					  {false, '$extensions', NewDict}),
		decode(R, Types, NewAcc);
	    _ ->
		{ok, Skipped} = protobuffs:skip_next_field(Bytes),
		decode(Skipped, Types, Acc)
	  end
    end.

unpack_value(Binary, string) when is_binary(Binary) ->
    binary_to_list(Binary);
unpack_value(Value, _) -> Value.

to_record(versiondummy, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       versiondummy),
						   Record, Name, Val)
			  end,
			  #versiondummy{}, DecodedTuples),
    Record1;
to_record(query, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields, query),
						   Record, Name, Val)
			  end,
			  #query{}, DecodedTuples),
    Record1;
to_record(frame, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields, frame),
						   Record, Name, Val)
			  end,
			  #frame{}, DecodedTuples),
    Record1;
to_record(backtrace, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       backtrace),
						   Record, Name, Val)
			  end,
			  #backtrace{}, DecodedTuples),
    Record1;
to_record(response, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       response),
						   Record, Name, Val)
			  end,
			  #response{}, DecodedTuples),
    Record1;
to_record(datum, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields, datum),
						   Record, Name, Val)
			  end,
			  #datum{}, DecodedTuples),
    decode_extensions(Record1);
to_record(term, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields, term),
						   Record, Name, Val)
			  end,
			  #term{}, DecodedTuples),
    decode_extensions(Record1);
to_record(query_assocpair, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       query_assocpair),
						   Record, Name, Val)
			  end,
			  #query_assocpair{}, DecodedTuples),
    Record1;
to_record(datum_assocpair, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       datum_assocpair),
						   Record, Name, Val)
			  end,
			  #datum_assocpair{}, DecodedTuples),
    Record1;
to_record(term_assocpair, DecodedTuples) ->
    Record1 = lists:foldr(fun ({_FNum, Name, Val},
			       Record) ->
				  set_record_field(record_info(fields,
							       term_assocpair),
						   Record, Name, Val)
			  end,
			  #term_assocpair{}, DecodedTuples),
    Record1.

decode_extensions(#datum{'$extensions' = Extensions} =
		      Record) ->
    Types = [],
    NewExtensions = decode_extensions(Types,
				      dict:to_list(Extensions), []),
    Record#datum{'$extensions' = NewExtensions};
decode_extensions(#term{'$extensions' = Extensions} =
		      Record) ->
    Types = [],
    NewExtensions = decode_extensions(Types,
				      dict:to_list(Extensions), []),
    Record#term{'$extensions' = NewExtensions};
decode_extensions(Record) -> Record.

decode_extensions(_Types, [], Acc) ->
    dict:from_list(Acc);
decode_extensions(Types, [{Fnum, Bytes} | Tail], Acc) ->
    NewAcc = case lists:keyfind(Fnum, 1, Types) of
	       {Fnum, Name, Type, Opts} ->
		   {Value1, Rest1} = case lists:member(is_record, Opts) of
				       true ->
					   {{FNum, V}, R} =
					       protobuffs:decode(Bytes, bytes),
					   RecVal = decode(Type, V),
					   {RecVal, R};
				       false ->
					   case lists:member(repeated_packed,
							     Opts)
					       of
					     true ->
						 {{FNum, V}, R} =
						     protobuffs:decode_packed(Bytes,
									      Type),
						 {V, R};
					     false ->
						 {{FNum, V}, R} =
						     protobuffs:decode(Bytes,
								       Type),
						 {unpack_value(V, Type), R}
					   end
				     end,
		   case lists:member(repeated, Opts) of
		     true ->
			 case lists:keytake(FNum, 1, Acc) of
			   {value, {FNum, Name, List}, Acc1} ->
			       decode(Rest1, Types,
				      [{FNum, Name,
					lists:reverse([int_to_enum(Type, Value1)
						       | lists:reverse(List)])}
				       | Acc1]);
			   false ->
			       decode(Rest1, Types,
				      [{FNum, Name, [int_to_enum(Type, Value1)]}
				       | Acc])
			 end;
		     false ->
			 [{Fnum,
			   {optional, int_to_enum(Type, Value1), Type, Opts}}
			  | Acc]
		   end;
	       false -> [{Fnum, Bytes} | Acc]
	     end,
    decode_extensions(Types, Tail, NewAcc).

set_record_field(Fields, Record, '$extensions',
		 Value) ->
    Decodable = [],
    NewValue = decode_extensions(element(1, Record),
				 Decodable, dict:to_list(Value)),
    Index = list_index('$extensions', Fields),
    erlang:setelement(Index + 1, Record, NewValue);
set_record_field(Fields, Record, Field, Value) ->
    Index = list_index(Field, Fields),
    erlang:setelement(Index + 1, Record, Value).

list_index(Target, List) -> list_index(Target, List, 1).

list_index(Target, [Target | _], Index) -> Index;
list_index(Target, [_ | Tail], Index) ->
    list_index(Target, Tail, Index + 1);
list_index(_, [], _) -> -1.

extension_size(#datum{'$extensions' = Extensions}) ->
    dict:size(Extensions);
extension_size(#term{'$extensions' = Extensions}) ->
    dict:size(Extensions);
extension_size(_) -> 0.

has_extension(_Record, _FieldName) -> false.

get_extension(#datum{'$extensions' = Extensions}, Int)
    when is_integer(Int) ->
    case dict:find(Int, Extensions) of
      {ok, {_Rule, Value, _Type, _Opts}} -> {ok, Value};
      {ok, Binary} -> {raw, Binary};
      error -> undefined
    end;
get_extension(#term{'$extensions' = Extensions}, Int)
    when is_integer(Int) ->
    case dict:find(Int, Extensions) of
      {ok, {_Rule, Value, _Type, _Opts}} -> {ok, Value};
      {ok, Binary} -> {raw, Binary};
      error -> undefined
    end;
get_extension(_Record, _FieldName) -> undefined.

set_extension(Record, _, _) -> {error, Record}.

