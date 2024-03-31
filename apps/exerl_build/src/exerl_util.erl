-module(exerl_util).

-export([
    ensure_started/1,
    ensure_loaded/1
]).

%% @doc Start an application and its dependencies and explicitly `error' in case
%% it can not be started.
-spec ensure_started(atom()) -> ok.
ensure_started(App) ->
    case application:ensure_all_started(App) of
        {ok, _} -> ok;
        {error, already_started} -> ok;
        {error, Error} -> error(Error)
    end.

-spec ensure_loaded([binary()]) -> ok.
ensure_loaded(Deps) ->
    rebar_api:debug("[exerl] Deps: ~p", [Deps]),
    CodePathAsMap = maps:from_list([
        {
            filename:basename(filename:dirname(P)),
            P
        }
     || P <- code:get_path()
    ]),

    Paths = lists:foldl(
        fun
            (Dep, Acc) when is_map_key(Dep, CodePathAsMap), is_list(Acc) ->
                [maps:get(Dep, CodePathAsMap) | Acc];
            (_, Acc) ->
                Acc
        end,
        [],
        Deps
    ),

    ModulesToLoad = [
        list_to_atom(Mod)
     || {Mod, ModPath, IsLoaded} <- code:all_available(),
        not IsLoaded,
        lists:any(
            fun(P) -> string:prefix(ModPath, P) =/= nomatch end,
            Paths
        )
    ],

    rebar_api:debug("[exerl] Loading dependency modules: ~p", [ModulesToLoad]),
    code:ensure_modules_loaded(ModulesToLoad),
    ok.
