-module(exerl).

-ignore_xref([init/1]).

-export([
    init/1
]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    os:putenv("MIX_ENV", "prod"),
    BaseDir = rebar_dir:base_dir(State),
    DepsDir = filename:join(BaseDir, "lib"),

    case application:get_key(rebar, vsn) of
        {ok, Vsn} ->
            {ok, Req} = verl:parse_requirement(<<">= 3.23.0">>),
            case verl:is_match(list_to_binary(Vsn), Req) of
                false ->
                    rebar_api:warn(
                        "Rebar3 version ~s does not support dependencies like "
                        "\"1.0 or 2.0\", please upgrade to at least 3.23.0 if "
                        "you are seeing errors that mention a version like this",
                        [Vsn]
                    );
                _ ->
                    ok
            end;
        _ ->
            ok
    end,

    try
        ScriptName = escript:script_name(),
        os:putenv("MIX_REBAR3", ensure_string(filename:absname(ScriptName)))
    catch
        error:_ ->
            ok
    end,

    os:putenv("MIX_DEPS_PATH", ensure_string(filename:absname(DepsDir))),
    os:putenv("MIX_BUILD_PATH", ensure_string(filename:absname(BaseDir))),

    % Delay the actual startup of the apps until the deps are loaded such that
    % we can load the elixir deps that are specified by the user!

    State1 = rebar_state:prepend_compilers(State, [exerl_elixir_compiler]),
    State2 = rebar_state:add_project_builder(State1, mix, exerl_mix_builder),
    {ok, State3} = exerl_prv_consolidate:init(State2),
    {ok, State4} = exerl_dep:init(State3),
    {ok, State5} = exerl_prv_iex:init(State4),

    {ok, State5}.

ensure_string(V) ->
    exerl_util:ensure_string(V).
