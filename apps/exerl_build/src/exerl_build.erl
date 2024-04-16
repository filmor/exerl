-module(exerl_build).

-export([
    init/1
]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    os:putenv("MIX_ENV", "prod"),
    BaseDir = rebar_dir:base_dir(State),
    DepsDir = filename:join(BaseDir, "lib"),

    try
        ScriptName = escript:script_name(),
        os:putenv("MIX_REBAR3", filename:absname(ScriptName))
    catch
        error:_ ->
            ok
    end,

    os:putenv("MIX_DEPS_PATH", ensure_string(filename:absname(DepsDir))),
    os:putenv("MIX_BUILD_PATH", ensure_string(filename:absname(BaseDir))),

    % TODO: Delay the actual startup of the apps until the deps are loaded such
    % that we can load the elixir deps that are specified by the user!

    State1 = rebar_state:prepend_compilers(State, [exerl_elixir_compiler]),
    State2 = rebar_state:add_project_builder(State1, mix, exerl_mix_builder),
    {ok, State3} = exerl_prv_consolidate:init(State2),

    {ok, State3}.

ensure_string(V) when is_binary(V) ->
    binary_to_list(V);
ensure_string(V) when is_list(V) ->
    V.
