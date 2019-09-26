-module(jamdb_oracle_SUITE).
-include_lib("common_test/include/ct.hrl").

-export([all/0, test/1]).

-define(DPI_MAJOR_VERSION, 3).
-define(DPI_MINOR_VERSION, 0).
-define(MAX_NUMBERS, 100000).

all() ->
	ct:pal("=[DEBUG]=> ~p:~p:~p", [?MODULE, ?FUNCTION_NAME, ?LINE]),
	[test].

test(_) ->
	ct:pal("=[DEBUG]=> ~p:~p:~p", [?MODULE, ?FUNCTION_NAME, ?LINE]),
	Opts = [
    	{host, ct:get_config(host)},
    	{port, 1521},
    	{user, ct:get_config(user)},
    	{password, ct:get_config(password)},
    	{service_name, ct:get_config(service_name)},
    	{app_name, "test"}
	],
	ct:pal("=[DEBUG]=> ~p:~p:~p", [?MODULE, ?FUNCTION_NAME, ?LINE]),
	Queries = [
		lists:flatten(
			io_lib:format(
				"insert into test(item) values('4179~7..0B')", [Count]
			)
		) || Count <- lists:seq(0, ?MAX_NUMBERS)
	],
	ct:pal("=[DEBUG]=> ~p:~p:~p", [?MODULE, ?FUNCTION_NAME, ?LINE]),
	try
		{Time, Result} = timer:tc(fun test_i/2, [Opts, Queries]),
		ct:pal(
			"~p rows inserted in ~p seconds",
			[?MAX_NUMBERS - Result, Time / 1000000]
		)
	catch
		Class:Error ->
			ct:pal("ERROR: ~p:~p~n~p", [Class, Error, erlang:get_stacktrace()])
	end.

test_i([{_,_} | _] = Opts, SQLs) ->
	ct:pal("=[DEBUG]=> ~p:~p:~p", [?MODULE, ?FUNCTION_NAME, ?LINE]),
	try
		{ok, ConnRef} = setup(Opts),
		ct:pal("=[DEBUG]=> ~p:~p:~p", [?MODULE, ?FUNCTION_NAME, ?LINE]),
		test_i(ConnRef, SQLs)
	catch
		Class:Exception ->
			ct:pal(
				"=[ERROR]=> ~p:~p:~p ~p:~p~n~p",
				[
					?MODULE, ?FUNCTION_NAME, ?LINE, Class, Exception,
					erlang:get_stacktrace()
				]
			)
	end;
test_i(ConnRef, []) ->
	ok = jamdb_oracle:stop(ConnRef),
	io:format(user, "~n", []),
	0;
test_i(ConnRef, [SQL | SQLs]) ->
	case jamdb_oracle:sql_query(ConnRef, SQL) of
		{ok, [{affected_rows, 1}]} ->
			SQLsLen = length(SQLs),
			if SQLsLen rem 100 == 0 ->
				io:format(user, " ~p", [SQLsLen]);
				true -> ok
			end,
			test_i(ConnRef, SQLs);
		{ok, [{proc_result, _, Error}]} ->
			ct:pal("===> Abort reason ~p", [Error]),
			length(SQLs) + 1
	end.

setup(Opts) ->
	ct:pal("=[DEBUG]=> ~p:~p:~p~n~p", [?MODULE, ?FUNCTION_NAME, ?LINE, Opts]),
	Result = (catch apply(jamdb_oracle, start_link, [Opts])),
	ct:pal("=[DEBUG]=> ~p:~p:~p~n~p", [?MODULE, ?FUNCTION_NAME, ?LINE, Result]),
	ct:pal("=[DEBUG]=> ~p:~p:~p~n~p", [?MODULE, ?FUNCTION_NAME, ?LINE]),
	{ok, ConnRef} = jamdb_oracle:start_link(Opts),
	ct:pal("===> Connect with:~n\tOpts ~p", [Opts]),
	{ok, []} = jamdb_oracle:sql_query(ConnRef, "COMON;"),
	case jamdb_oracle:sql_query(ConnRef, "drop table test") of
		{ok,[{proc_result, 942, _}]} -> ok;
		{ok,[{affected_rows,0}]} -> ok
	end,
	{ok,[{affected_rows,0}]} = jamdb_oracle:sql_query(
		ConnRef, "create table test (item varchar(1000))"
	),
	{ok, ConnRef}.
