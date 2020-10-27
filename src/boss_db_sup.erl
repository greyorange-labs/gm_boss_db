-module(boss_db_sup).
-author('emmiller@gmail.com').

-behaviour(supervisor).

-export([start_link/0, start_link/1]).

-export([init/1]).

start_link() ->
    start_link([]).

start_link(StartArgs) ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, StartArgs).

init(StartArgs) ->
    Args = [{name, {local, boss_db_pool}},
                         {worker_module, boss_db_controller},
                         {size, 5}, {max_overflow, 10}] ++ StartArgs,
    PoolSpec = {db_controller, {poolboy, start_link, [Args]}, permanent, 2000, worker, [poolboy]},
    AppPools = proplists:get_value(app_pools, StartArgs, #{}),
    ExtraPoolSpec =
        maps:fold(
            fun(App, AppSpec, SpecAcc) ->
                case maps:get(boss_db_pool, AppSpec, undefined) of
                    undefined -> SpecAcc;
                    SizeSpec ->
                        PoolName = list_to_atom(atom_to_list(App) ++ "_boss_db_pool"),
                        PoolArgs = [{name, {local, PoolName}},
                                {worker_module, boss_db_controller},
                                {size, 5}, {max_overflow, 10}] ++ StartArgs ++ SizeSpec,
                        [{list_to_atom(atom_to_list(App) ++ "_db_controller"),
                            {poolboy, start_link, [PoolArgs]}, permanent, 2000, worker, [poolboy]} | SpecAcc]
                end
            end,
            [],
            AppPools
        ),
    {ok, {{one_for_one, 10, 10}, [PoolSpec | ExtraPoolSpec]}}.
