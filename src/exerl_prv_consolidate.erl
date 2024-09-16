-module(exerl_prv_consolidate).

-include("exerl.hrl").

-export([
    init/1,
    do/1,
    format_error/1
]).

-behaviour(provider).

-define(PROVIDER, consolidate_protocols).
-define(DEPS, [{default, compile}]).

%%%===================================================================
%%% API
%%%===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
        %% The 'user friendly' name of the task
        {name, ?PROVIDER},
        %% The module implementation of the task
        {module, ?MODULE},
        %% The task can be run by the user, always true
        {bare, true},
        %% The list of dependencies
        {deps, ?DEPS},
        %% How to use the plugin
        {example, "rebar3 consolidate_protocols"},
        %% list of options understood by the plugin
        {opts, []},
        {short_desc, "Consolidate protocols"},
        {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    exerl_util:ensure_elixir(),

    Cwd = rebar_state:dir(State),
    Providers = rebar_state:providers(State),
    rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, State),
    Deps = rebar_state:all_deps(State),
    ProjectApps = rebar_state:project_apps(State),

    TargetApp = hd(ProjectApps),
    TargetAppName = ensure_atom(rebar_app_info:name(TargetApp)),
    OutDir = rebar_app_info:ebin_dir(TargetApp),
    filelib:ensure_path(OutDir),

    AppPathsFromDeps = maps:from_list([
        {ensure_atom(rebar_app_info:name(A)), rebar_app_info:ebin_dir(A)}
     || A <- ProjectApps ++ Deps
    ]),

    % Through adding the code path (which has previously been updated with
    % Elixir's paths), this will also consolidate the builtin protocols.
    AppPathsFromCode = maps:from_list([{get_app_name(P), P} || P <- code:get_path()]),

    AppPaths = maps:without(
        [TargetAppName, '$unused_app'],
        maps:merge(AppPathsFromCode, AppPathsFromDeps)
    ),

    Paths = lists:uniq(maps:values(AppPaths)),

    Protos = maps:filtermap(
        fun(_, Path) ->
            case ?Protocol:extract_protocols([Path]) of
                [] ->
                    false;
                P ->
                    {true, P}
            end
        end,
        AppPaths
    ),

    rebar_api:info(
        "Consolidating ~p protocols ...",
        [length(lists:flatten(maps:values(Protos)))]
    ),

    maps:foreach(
        fun(App, PerAppProtos) ->
            lists:foreach(
                fun(Proto) ->
                    Impls = ?Protocol:extract_impls(Proto, Paths),
                    rebar_api:debug("Implementations of ~p from ~p:~n~p", [Proto, App, Impls]),

                    {ok, Consolidated} = ?Protocol:consolidate(Proto, Impls),

                    Name = filename:join(OutDir, atom_to_list(Proto) ++ ".beam"),
                    file:write_file(Name, Consolidated),
                    ok
                end,
                PerAppProtos
            )
        end,
        Protos
    ),

    rebar_hooks:run_all_hooks(
        Cwd, post, ?PROVIDER, Providers, State
    ),

    State1 = update_relx_excludes(Protos, State),

    {ok, State1}.

update_relx_excludes(Protos, State) ->
    Relx = rebar_state:get(State, relx, []),
    ExcludeModules = proplists:get_value(exclude_modules, Relx, []),
    ExcludeModules1 = proplists:from_map(Protos) ++ ExcludeModules,

    Relx1 = [{exclude_modules, ExcludeModules1} | Relx],

    rebar_state:set(State, relx, Relx1).

get_app_name(EbinDir) ->
    try
        [AppFilename | _] = filelib:wildcard("*.app", EbinDir),
        AppName = filename:rootname(AppFilename),
        list_to_existing_atom(AppName)
    catch
        error:_ ->
            '$unused_app'
    end.

ensure_atom(String) when is_list(String) ->
    list_to_atom(String);
ensure_atom(Binary) when is_binary(Binary) ->
    binary_to_atom(Binary);
ensure_atom(Atom) when is_atom(Atom) ->
    Atom.
-spec format_error(any()) -> iolist().
format_error(no_app) ->
    io_lib:format("No so_name or application defined.", []);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
