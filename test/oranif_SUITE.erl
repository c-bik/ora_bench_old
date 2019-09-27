-module(oranif_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, test/1]).

-define(DPI_MAJOR_VERSION, 3).
-define(DPI_MINOR_VERSION, 0).
-define(MAX_VARS, 100000).

all() ->
	dpi:load_unsafe(),
	[test].

test(_) ->
	ConnFmt = ct:get_config(connection),
	User = l2b(ct:get_config(user)),
	Password = l2b(ct:get_config(password)),
	Connection = l2b(
		io_lib:format(
			ConnFmt, [
				ct:get_config(host), ct:get_config(port),
				ct:get_config(service_name)
			]
		)
	),

	try
		setup(Connection, User, Password),
		{Time, Inserted} = timer:tc(
			fun test_i/3, [Connection, User, Password]
		),
		Seconds = Time / 1000000,
		ct:pal("ORANIF inserted ~p rows in ~p seconds", [Inserted, Seconds])
	catch
		Class:Error ->
		ct:pal("ERROR: ~p:~p~n~p", [Class, Error, erlang:get_stacktrace()])
	end.

test_i(Connection, User, Password) ->
	Ctx = dpi:context_create(?DPI_MAJOR_VERSION, ?DPI_MINOR_VERSION),
	Conn = dpi:conn_create(
		Ctx, User, Password, Connection,
		#{encoding => "AL32UTF8", nencoding => "AL32UTF8"}, #{}
	),

	#{var := ItemVar} = dpi:conn_newVar(
		Conn, 'DPI_ORACLE_TYPE_VARCHAR', 'DPI_NATIVE_TYPE_BYTES',
		?MAX_VARS, 16, true, false, null
	),

	Stmt = dpi:conn_prepareStmt(
	Conn, false, l2b(ct:get_config(insert_oranif)), <<>>
	),
	dpi:stmt_bindByName(Stmt, <<"ITEM">>, ItemVar),
	Inserted = set_from_bytes(ItemVar, Stmt),

	dpi:conn_commit(Conn),
	dpi:stmt_close(Stmt, <<>>),
	dpi:var_release(ItemVar),
	dpi:conn_close(Conn, [], <<>>),
	dpi:context_destroy(Ctx),
	Inserted.

l2b(B) when is_binary(B) -> B;
l2b(L) when is_list(L) -> list_to_binary(L).

set_from_bytes(ItemVar, Stmt) -> set_from_bytes(0, 0, ItemVar, Stmt).
set_from_bytes(Last, Count, ItemVar, Stmt) when Count - Last >= ?MAX_VARS ->
	case catch dpi:stmt_executeMany(Stmt, [], ?MAX_VARS) of
		ok ->
			io:format(user, " ~p", [Count]),
			set_from_bytes(Count, Count, ItemVar, Stmt);
		_ ->
			io:format(user, "~n", []),
			Count
	end;
set_from_bytes(LastCount, Count, ItemVar, Stmt) ->
	Item = list_to_binary(io_lib:format("~10..0B", [Count])),
	dpi:var_setFromBytes(ItemVar, Count, Item),
	set_from_bytes(LastCount, Count + 1, ItemVar, Stmt).

setup(Connection, User, Password) ->
	Ctx = dpi:context_create(?DPI_MAJOR_VERSION, ?DPI_MINOR_VERSION),
	Conn = dpi:conn_create(
		Ctx, User, Password, Connection,
		#{encoding => "AL32UTF8", nencoding => "AL32UTF8"}, #{}
	),

	DropStmt = dpi:conn_prepareStmt(Conn, false, l2b(ct:get_config(drop)), <<>>),
	case catch dpi:stmt_execute(DropStmt, []) of
		0 -> ok;
		{'EXIT', {{error, _, _, #{code := 942}}, _}} -> ok;
		BadError ->
			ct:pal("===> ~p:~p SHOULDN'T happen ~p", [?MODULE, ?LINE, BadError])
	end,
	dpi:stmt_close(DropStmt, <<>>),

	CreateStmt = dpi:conn_prepareStmt(
		Conn, false, l2b(ct:get_config(create)), <<>>
	),
	0 = dpi:stmt_execute(CreateStmt, []),
	dpi:stmt_close(CreateStmt, <<>>),

	dpi:conn_commit(Conn),
	dpi:conn_close(Conn, [], <<>>),
	dpi:context_destroy(Ctx).
