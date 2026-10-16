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

    case ProjectApps of
        [] ->
            rebar_api:error("No project apps found, no place to put consolidated protocols", []),
            {ok, State};
        [FirstApp | _] = ProjectApps ->
            OutDir = rebar_app_info:ebin_dir(FirstApp),
            filelib:ensure_path(OutDir),
            DepPaths = [rebar_app_info:ebin_dir(App) || App <- Deps],
            remove_stale_protocols(OutDir, DepPaths),

            % Through adding the code path (which has previously been updated with
            % Elixir's paths), this will also consolidate the builtin protocols.
            Paths = lists:uniq(
                [rebar_app_info:ebin_dir(A) || A <- ProjectApps ++ Deps] ++ code:get_path()
            ),
            Protos = lists:uniq(?Protocol:extract_protocols(Paths)),

            rebar_api:info("Consolidating ~p protocols ...", [length(Protos)]),

            lists:foreach(
                fun(Proto) ->
                    Impls = ?Protocol:extract_impls(Proto, Paths),
                    rebar_api:debug("Implementations of ~p:~n~p", [Proto, Impls]),

                    Consolidated =
                        case ?Protocol:consolidate(Proto, Impls) of
                            {ok, Binary} ->
                                Binary;
                            {error, Reason} ->
                                rebar_api:abort(
                                    "Cannot consolidate protocol ~p from ~p: ~p",
                                    [Proto, code:which(Proto), Reason]
                                )
                        end,

                    Name = filename:join(OutDir, atom_to_list(Proto) ++ ".beam"),
                    ok = file:write_file(Name, Consolidated),
                    ok
                end,
                Protos
            ),

            rebar_hooks:run_all_hooks(
                Cwd, post, ?PROVIDER, Providers, State
            ),

            {ok, State}
    end.

remove_stale_protocols(OutDir, DepPaths) ->
    DepProtocols = ?Protocol:extract_protocols(DepPaths),
    lists:foreach(
        fun(Protocol) ->
            Name = atom_to_list(Protocol) ++ ".beam",
            Output = filename:join(OutDir, Name),
            Originals = [
                filename:join(Path, Name)
             || Path <- DepPaths,
                filename:absname(Path) =/= filename:absname(OutDir),
                filelib:is_regular(filename:join(Path, Name))
            ],
            case Originals of
                [Original | _] ->
                    % Preserve project protocols that intentionally shadow a dependency module.
                    case beam_source(Output) =:= beam_source(Original) of
                        true ->
                            rebar_api:info("Removing stale consolidated protocol ~p", [Protocol]),
                            ok = file:delete(Output),
                            case code:is_loaded(Protocol) of
                                {file, Output} ->
                                    {module, Protocol} = code:load_abs(filename:rootname(Original));
                                _ ->
                                    ok
                            end;
                        false ->
                            ok
                    end;
                [] ->
                    ok
            end
        end,
        ?Protocol:extract_protocols([OutDir]) -- DepProtocols
    ).

beam_source(Path) ->
    {ok, {_, [{compile_info, Info}]}} = beam_lib:chunks(Path, [compile_info]),
    {source, Source} = lists:keyfind(source, 1, Info),
    Source.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
