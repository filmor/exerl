-module(exerl).

-export([
    find/0,
    set_code_path/0,
    set_code_path/1,
    start/0,
    start_mix/0,
    compile/2
]).

-export([init/1]).

find() ->
    exerl_find:from_executable().

set_code_path() ->
    set_code_path(find()).

set_code_path(Path) ->
    case code:which(elixir) of
        non_existing ->
            Paths = filelib:wildcard(
                binary_to_list(
                    filename:join([Path, <<"*">>, <<"ebin">>])
                )
            ),
            code:add_pathsz(Paths),
            ok;
        _ ->
            already_added
    end.

start() ->
    set_code_path(),
    {ok, _} = application:ensure_all_started(elixir),
    % Prevent Elixir from messing with Erlang's handler config
    application:load(logger),
    application:set_env(logger, handle_otp_reports, false),
    {ok, _} = application:ensure_all_started(logger),
    ok.

start_mix() ->
    start(),
    {ok, _} = application:ensure_all_started(mix),
    ok.

compile(Paths, Dest) ->
    exerl_compile:compile(Paths, Dest).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    % os:putenv("MIX_ENV", "prod"),
    os:putenv("MIX_DEPS_PATH", filename:absname("./_build/default/lib")),
    os:putenv("MIX_BUILD_PATH", filename:absname("./_build/default")),

    % TODO: Either ensure that dependencies are used (resources) or gather from
    % executable.
    %
    % set_code_path(),
    %
    % TODO: Load on-demand instead
    % {ok, _} = application:ensure_all_started(mix),
    State1 = rebar_state:prepend_compilers(State, [exerl_r3_compile]),
    State2 = rebar_state:add_resource(State1, {elixir, exerl_r3_resource}),
    State3 = rebar_state:add_project_builder(State2, mix, exerl_r3_builder),
    % exerl_mix_converger:register(),
    {ok, State3}.
