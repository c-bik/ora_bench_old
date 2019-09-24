-module(oranif_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, test/1]).

-define(DPI_MAJOR_VERSION, 3).
-define(DPI_MINOR_VERSION, 0).
-define(MAX_NUMBERS, 100000).

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
		{Time, Result} = timer:tc(fun test_i/3, [Connection, User, Password]),
		if Result /= ok -> ct:pal("ERROR ~p\n", [Result]); true -> ok end,
		ct:pal(
			"~p rows inserted in ~p seconds",
			[?MAX_NUMBERS, Time / 1000000]
		)
	catch
		Class:Error ->
		ct:pal("ERROR: ~p:~p~n~p", [Class, Error, erlang:get_stacktrace()])
	end.

test_i(Connection, User, Password) ->
	ct:pal(
		"===> Connect with:~n\tConnection ~s~n\tUser ~s~n\tPassword ~s",
		[Connection, User, Password]
	),
	Ctx = dpi:context_create(?DPI_MAJOR_VERSION, ?DPI_MINOR_VERSION),
	Conn = dpi:conn_create(
		Ctx, User, Password, Connection,
		#{encoding => "AL32UTF8", nencoding => "AL32UTF8"}, #{}
	),

	#{var := ItemVar} = dpi:conn_newVar(
	Conn, 'DPI_ORACLE_TYPE_VARCHAR', 'DPI_NATIVE_TYPE_BYTES',
	?MAX_NUMBERS, 16, true, false, null
	),

	Stmt = dpi:conn_prepareStmt(
	Conn, false, <<"insert into test (ITEM) values (:ITEM)">>, <<>>
	),
	dpi:stmt_bindByName(Stmt, <<"ITEM">>, ItemVar),
	set_from_bytes(ItemVar),
	dpi:stmt_executeMany(Stmt, [], ?MAX_NUMBERS),

	dpi:conn_commit(Conn),
	dpi:stmt_close(Stmt, <<>>),
	dpi:var_release(ItemVar),
	dpi:conn_close(Conn, [], <<>>),
	dpi:context_destroy(Ctx).

l2b(B) when is_binary(B) -> B;
l2b(L) when is_list(L) -> list_to_binary(L).

set_from_bytes(ItemVar) -> set_from_bytes(0, ItemVar).
set_from_bytes(Count, _) when Count >= ?MAX_NUMBERS -> ok;
set_from_bytes(Count, ItemVar) ->
	Item = list_to_binary(io_lib:format("4179~7..0B", [Count])),
	dpi:var_setFromBytes(ItemVar, Count, Item),
	set_from_bytes(Count + 1, ItemVar).

setup(Connection, User, Password) ->
	Ctx = dpi:context_create(?DPI_MAJOR_VERSION, ?DPI_MINOR_VERSION),
	Conn = dpi:conn_create(
		Ctx, User, Password, Connection,
		#{encoding => "AL32UTF8", nencoding => "AL32UTF8"}, #{}
	),

	DropStmt = dpi:conn_prepareStmt(Conn, false, <<"drop table test">>, <<>>),
	0 = dpi:stmt_execute(DropStmt, []),
	dpi:stmt_close(DropStmt, <<>>),

	CreateStmt = dpi:conn_prepareStmt(
		Conn, false, <<"create table test (item varchar(1000))">>, <<>>
	),
	0 = dpi:stmt_execute(CreateStmt, []),
	dpi:stmt_close(CreateStmt, <<>>),

	dpi:conn_commit(Conn),
	dpi:conn_close(Conn, [], <<>>),
	dpi:context_destroy(Ctx).
