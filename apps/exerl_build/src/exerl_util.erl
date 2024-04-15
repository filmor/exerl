-module(exerl_util).

-export([
    ensure_elixir/0,
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

-spec ensure_elixir() -> ok.
ensure_elixir() ->
    % TODO: Check if path changed, if it did, unload everything
    exerl_find:set_code_path(),
    try
        exerl_util:ensure_started(logger),
        exerl_util:ensure_started(mix),
        exerl_util:ensure_started(eex)
        % exerl_util:ensure_started(ex_unit)
    catch
        error:Err ->
            rebar_api:abort(
                "[exerl] Failed to load Elixir, please add it to the dependency list (~p)", [Err]
            )
    end,

    DebugLevel = rebar_log:debug_level(),
    DiagLevel = rebar_log:diagnostic_level(),
    Debug =
        case rebar_log:get_level() of
            DebugLevel ->
                true;
            DiagLevel ->
                true;
            _ ->
                false
        end,

    % Set debug output for mix
    'Elixir.Mix':debug(Debug),

    exerl_mix_converger:register(),
    'Elixir.Mix.SCM':prepend(exerl_mix_scm),

    % Ensure Elixir modules that are used for their macros are loaded:
    exerl_util:ensure_loaded(["logger", "mix", "elixir", "eex", "ex_unit"]).


-spec ensure_loaded([binary()]) -> ok.
ensure_loaded(Deps) ->
    rebar_api:debug("[exerl] Deps: ~p", [Deps]),
    % TODO: Ensure that all are part of the code path!
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
