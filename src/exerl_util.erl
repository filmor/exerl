-module(exerl_util).

-include("exerl.hrl").

-export([
    ensure_elixir/0,
    ensure_started/1,
    ensure_loaded/1,
    ensure_string/1
]).

%% @doc Start an application and its dependencies and explicitly `error' in case
%% it can not be started.
-spec ensure_started(atom()) -> ok.
ensure_started(App) ->
    case application:ensure_all_started(App) of
        {ok, _} -> ok;
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
    ?Mix:debug(Debug),

    exerl_mix_converger:register(),
    ?Scm:prepend(exerl_mix_scm),

    ok.

-spec ensure_loaded([binary()]) -> ok.
ensure_loaded(Deps) ->
    rebar_api:debug("[exerl] Deps: ~p", [Deps]),

    % TODO: Ensure that all are part of the code path!
    CodePathAsMap = maps:from_list([
        {filename:basename(filename:dirname(P)), P}
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
        % elp:ignore W0023 (atoms_exhaustion)
        list_to_atom(Mod)
     || {Mod, ModPath, IsLoaded} <- code:all_available(),
        not IsLoaded,
        % ModPath may be 'precompiled' or 'cover_compiled' in some cases, so filter
        % these here (see code:is_loaded/1)
        if
            is_atom(ModPath) ->
                false;
            true ->
                lists:any(
                    fun(P) -> string:prefix(ModPath, P) =/= nomatch end,
                    Paths
                )
        end
    ],

    rebar_api:debug("[exerl] Loading dependency modules: ~p", [ModulesToLoad]),
    code:ensure_modules_loaded(ModulesToLoad),
    ok.

-spec ensure_string(string() | binary() | list() | atom()) -> string().
ensure_string(V) when is_binary(V) ->
    binary_to_list(V);
ensure_string(V) when is_list(V) ->
    lists:flatten(V);
ensure_string(V) when is_atom(V) ->
    atom_to_list(V).
