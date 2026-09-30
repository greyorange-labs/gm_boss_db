-module(boss_pool).
-export([call/2, call/3, checkout_connected_worker/1]).

-define(MAXDELAY, 60000).
-define(CONNECTION_TIMEOUT_SEED, 1000).
-define(GENSERVER_TIMEOUT, (30 * 1000)).

call(Pool, Msg) ->
    Worker = poolboy:checkout(Pool),
    try
        gen_server:call(Worker, Msg)
    catch
        Class:Reason:Stacktrace ->
            lager:error("#boss_db_transaction_failure call/2 failed worker=~p pool=~p ~p:~p~n~p",
                        [Worker, Pool, Class, Reason, Stacktrace]),
            erlang:raise(Class, Reason, Stacktrace)
    after
        poolboy:checkin(Pool, Worker)
    end.

call(Pool, Msg, Timeout) ->
    case checkout_connected_worker(Pool) of
        {ok, Worker} ->
            try
                gen_server:call(Worker, Msg, Timeout)
            catch
                Class:Reason:Stacktrace ->
                    lager:error("#boss_db_transaction_failure call/3 failed worker=~p pool=~p ~p:~p~n~p",
                                [Worker, Pool, Class, Reason, Stacktrace]),
                    erlang:raise(Class, Reason, Stacktrace)
            after
                poolboy:checkin(Pool, Worker)
            end;
        Response -> Response
    end.

%% @doc automatically checks in a worker if couldn't succeed in finding a connected one
checkout_connected_worker(Pool) ->
    Worker = poolboy:checkout(Pool, true, ?GENSERVER_TIMEOUT),
    try
        case wait_until_connected(Worker) of
            {ok, connected} -> {ok, Worker};
            Response ->
                poolboy:checkin(Pool, Worker),
                Response
        end
    catch
        Class:Reason:Stacktrace ->
            poolboy:checkin(Pool, Worker),
            lager:error("#boss_db_transaction_failure checkout_connected_worker failed worker=~p pool=~p ~p:~p~n~p",
                        [Worker, Pool, Class, Reason, Stacktrace]),
            erlang:raise(Class, Reason, Stacktrace)
    end.

wait_until_connected(Worker) ->
    wait_until_connected(Worker, ?CONNECTION_TIMEOUT_SEED).

wait_until_connected(Worker, Timeout) ->
    case gen_server:call(Worker, {get_connection_state}, ?GENSERVER_TIMEOUT) of
        connected ->
            {ok, connected};
        _ ->
            case Timeout >= ?MAXDELAY of
                true ->
                    lager:error("Connection retry limit reached for worker: ~p", [Worker]),
                    {error, connection_retry_limit_exceeded};
                _ ->
                    lager:warning("Boss pool worker: ~p not connected. Retrying in ~p", [Worker, Timeout*2]),
                    timer:sleep(Timeout*2),
                    wait_until_connected(Worker, Timeout*2)
            end
    end.
