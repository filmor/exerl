-module(exerl).

-ignore_xref([init/1]).

-export([
    init/1
]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    ensure_rebar_version(),

    os:putenv("MIX_ENV", "prod"),
    BaseDir = rebar_dir:base_dir(State),
    DepsDir = filename:join(BaseDir, "lib"),

    try
        ScriptName = escript:script_name(),
        os:putenv("MIX_REBAR3", ensure_string(filename:absname(ScriptName)))
    catch
        error:_ ->
            ok
    end,

    os:putenv("MIX_DEPS_PATH", ensure_string(filename:absname(DepsDir))),
    os:putenv("MIX_BUILD_PATH", ensure_string(filename:absname(BaseDir))),

    ok = exerl_mix_metadata:prepare(State),

    % Delay the actual startup of the apps until the deps are loaded such that
    % we can load the elixir deps that are specified by the user!

    State1 = rebar_state:prepend_compilers(State, [exerl_elixir_compiler]),
    State2 = rebar_state:add_project_builder(State1, mix, exerl_mix_builder),
    {ok, State3} = exerl_prv_consolidate:init(State2),
    {ok, State4} = exerl_dep:init(State3),
    {ok, State5} = exerl_prv_iex:init(State4),

    State6 = rebar_state:add_resource(State5, {git, exerl_mix_resource}),
    State7 = rebar_state:add_resource(State6, {git_subdir, exerl_mix_resource}),
    {ok, State7}.

ensure_rebar_version() ->
    case application:get_key(rebar, vsn) of
        {ok, Vsn} when is_list(Vsn) ->
            case code:ensure_loaded(rebar_semver) of
                {module, rebar_semver} ->
                    {ok, Version} = rebar_semver:parse_version(list_to_binary(Vsn)),
                    {ok, Minimum} = rebar_semver:parse_constraint(<<">= 3.26.0">>),
                    case rebar_semver:match(Version, Minimum) of
                        true ->
                            ok;
                        false ->
                            unsupported_rebar_version(Vsn)
                    end;
                {error, _} ->
                    unsupported_rebar_version(Vsn)
            end;
        _ ->
            rebar_api:abort("[exerl] Unable to determine the Rebar3 version", [])
    end.

unsupported_rebar_version(Vsn) ->
    rebar_api:abort(
        "[exerl] Rebar3 version ~s is not supported; "
        "upgrade to at least 3.26.0",
        [Vsn]
    ).

ensure_string(V) ->
    exerl_util:ensure_string(V).
