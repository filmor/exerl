-module(rebar3_exerl_build).

-export([
    init/1
        ]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    os:putenv("MIX_ENV", "prod"),
    BaseDir = rebar_dir:base_dir(State),
    DepsDir = rebar_dir:deps_dir(State),
    os:putenv("MIX_DEPS_PATH", filename:absname(DepsDir)),
    os:putenv("MIX_BUILD_PATH", filename:absname(BaseDir)),

    {ok, _} = application:ensure_all_started(mix),
    State1 = rebar_state:prepend_compilers(State, [exerl_r3_compile]),
    State2 = rebar_state:add_project_builder(State1, mix, exerl_r3_builder),
    % exerl_mix_converger:register(),
    {ok, State2}.

