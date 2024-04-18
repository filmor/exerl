-module(exerl_prv_iex).

-include("exerl.hrl").

-export([
    init/1,
    do/1,
    format_error/1
]).

-behaviour(provider).

-define(PROVIDER, iex).
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
        {example, "rebar3 iex"},
        %% list of options understood by the plugin
        {opts, []},
        {short_desc, "Start an Elixir shell"},
        {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    Cwd = rebar_state:dir(State),
    Providers = rebar_state:providers(State),
    rebar_hooks:run_all_hooks(Cwd, pre, ?PROVIDER, Providers, State),

    exerl_mix_converger:unregister(),

    exerl_find:set_code_path(),
    {ok, _} = application:ensure_all_started(elixir),
    {ok, _} = application:ensure_all_started(iex),

    ok = shell:start_interactive({'Elixir.IEx', start, [[{on_eof, halt}]]}),

    receive after 10000 -> ok end,

    case shell:whereis() of
        undefined ->
            error(failed_to_start_iex);
        Pid ->
            Ref = monitor(process, Pid),

            receive {'DOWN', Ref, process, _, _} -> ok end
    end,

    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(no_app) ->
    io_lib:format("No so_name or application defined.", []);
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
