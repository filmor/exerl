-module(rebar3_exerl_build).

-export([
    init/1
]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    os:putenv("MIX_ENV", "prod"),
    BaseDir = rebar_dir:base_dir(State),
    DepsDir = filename:join(BaseDir, "lib"),

    os:putenv("MIX_DEPS_PATH", ensure_string(filename:absname(DepsDir))),
    os:putenv("MIX_BUILD_PATH", ensure_string(filename:absname(BaseDir))),

    {ok, _} = application:ensure_all_started(mix),
    State1 = rebar_state:prepend_compilers(State, [exerl_r3_compile]),
    State2 = rebar_state:add_project_builder(State1, mix, exerl_r3_builder),
    exerl_mix_converger:register(),
    'Elixir.Mix.SCM':prepend(exerl_mix_scm),
    
    {ok, State2}.

ensure_string(V) when is_binary(V) ->
    binary_to_list(V);
ensure_string(V) when is_list(V) ->
    V.
