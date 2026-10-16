-module(exerl_mix_metadata_tests).

-include_lib("eunit/include/eunit.hrl").

-spec metadata_test_() -> term().
metadata_test_() ->
    {foreach, fun setup/0, fun cleanup/1, [
        fun(Dir) -> {timeout, 60, fun() -> generates_dependencies(Dir) end} end,
        fun(Dir) -> {timeout, 60, fun() -> preserves_config(Dir) end} end,
        fun(Dir) -> {timeout, 60, fun() -> prepares_checkout(Dir) end} end,
        fun(Dir) -> {timeout, 60, fun() -> prepares_cached_subdir(Dir) end} end,
        fun(Dir) -> {timeout, 60, fun() -> restores_mix_state(Dir) end} end,
        fun(Dir) -> {timeout, 60, fun() -> prepares_git_dependency(Dir) end} end
    ]}.

setup() ->
    Dir = filename:absname(
        filename:join(
            "_build", "metadata_test_" ++ integer_to_list(erlang:unique_integer([positive]))
        )
    ),
    ok = filelib:ensure_dir(filename:join(Dir, "mix.exs")),
    write_project(Dir, "0.1.0"),
    Dir.

cleanup(Dir) ->
    ok = file:del_dir_r(Dir).

write_project(Dir, Version) ->
    Template = read_fixture("mix.exs"),
    ok = file:write_file(
        filename:join(Dir, "mix.exs"),
        binary:replace(Template, ~"VERSION", list_to_binary(Version))
    ).

read_fixture(Name) ->
    Source = proplists:get_value(source, ?MODULE:module_info(compile)),
    Path = filename:join([filename:dirname(Source), "exerl_mix_metadata_tests_data", Name]),
    {ok, Data} = file:read_file(Path),
    Data.

generates_dependencies(Dir) ->
    ok = exerl_mix_metadata:generate(Dir),
    {ok, [{deps, Deps}]} = file:consult(filename:join(Dir, "rebar.config")),
    ?assertEqual([{runtime_app, "~> 1.0", {pkg, "runtime_package"}}], Deps),
    {ok, Metadata} = file:consult(filename:join(Dir, "hex_metadata.config")),
    ?assertEqual(~"0.1.0", proplists:get_value(~"version", Metadata)),
    ?assertEqual([~"mix"], proplists:get_value(~"build_tools", Metadata)),
    Requirements = [
        proplists:get_value(~"name", Requirement)
     || Requirement <- proplists:get_value(~"requirements", Metadata)
    ],
    ?assert(lists:member(~"runtime_package", Requirements)),
    ?assertNot(lists:member(~"dev_dep", Requirements)),
    write_project(Dir, "0.2.0"),
    ok = exerl_mix_metadata:generate(Dir),
    {ok, Updated} = file:consult(filename:join(Dir, "hex_metadata.config")),
    ?assertEqual(~"0.2.0", proplists:get_value(~"version", Updated)).

preserves_config(Dir) ->
    Config = filename:join(Dir, "rebar.config"),
    Original = ~"{deps, [existing_dep]}.\n",
    ok = file:write_file(Config, Original),
    ok = exerl_mix_metadata:generate(Dir),
    ?assertEqual({ok, Original}, file:read_file(Config)),
    ?assertNot(filelib:is_regular(filename:join(Dir, "hex_metadata.config"))),
    ok = file:delete(Config),
    ok = file:write_file(Config ++ ".script", ~"CONFIG.\n"),
    ok = exerl_mix_metadata:generate(Dir),
    ?assertNot(filelib:is_regular(Config)),
    ?assertNot(filelib:is_regular(filename:join(Dir, "hex_metadata.config"))).

prepares_checkout(Dir) ->
    Checkout = filename:join([Dir, "_checkouts", "metadata_fixture"]),
    ok = filelib:ensure_dir(filename:join(Checkout, "mix.exs")),
    write_project(Checkout, "0.1.0"),
    State = rebar_state:new([
        {root_dir, Dir},
        {base_dir, filename:join(Dir, "_build")}
    ]),
    ok = exerl_mix_metadata:prepare(State),
    {ok, AppInfo} = rebar_app_info:discover(Checkout, State),
    ?assertEqual(mix, rebar_app_info:project_type(AppInfo)),
    ?assertEqual(
        [{runtime_app, "~> 1.0", {pkg, "runtime_package"}}],
        rebar_app_info:get(AppInfo, {deps, default}, [])
    ).

prepares_cached_subdir(Dir) ->
    State0 = rebar_state:new([
        {root_dir, Dir},
        {base_dir, filename:join(Dir, "_build")}
    ]),
    Project = filename:join([rebar_dir:deps_dir(State0), "metadata_fixture", "apps/fixture"]),
    ok = filelib:ensure_dir(filename:join(Project, "mix.exs")),
    write_project(Project, "0.1.0"),
    Source = {git_subdir, "https://example.invalid/fixture", {ref, "HEAD"}, "apps/fixture"},
    State = rebar_state:set(State0, {locks, default}, [{~"metadata_fixture", Source, 0}]),
    ok = exerl_mix_metadata:prepare(State),
    {ok, [{deps, Deps}]} = file:consult(filename:join(Project, "rebar.config")),
    ?assertEqual([{runtime_app, "~> 1.0", {pkg, "runtime_package"}}], Deps).

restores_mix_state(Dir) ->
    exerl_util:ensure_elixir(),
    {ok, Cwd} = file:get_cwd(),
    Env = 'Elixir.Mix':env(),
    Undefined = 'Elixir.Code':get_compiler_option(no_warn_undefined),
    'Elixir.Mix.ProjectStack':on_clean_slate(fun() ->
        'Elixir.Mix.Project':push(nil, ~"caller/mix.exs"),
        Project = 'Elixir.Mix.ProjectStack':peek(),
        try
            'Elixir.Mix':env(dev),
            'Elixir.Code':put_compiler_option(no_warn_undefined, []),
            ok = exerl_mix_metadata:generate(Dir),
            ?assertEqual(Project, 'Elixir.Mix.ProjectStack':peek()),
            ?assertEqual(dev, 'Elixir.Mix':env()),
            ?assertEqual([], 'Elixir.Code':get_compiler_option(no_warn_undefined)),
            ?assertEqual({ok, Cwd}, file:get_cwd()),
            ?assertEqual(false, code:is_loaded('Elixir.MetadataFixture.MixProject')),
            ok = file:write_file(
                filename:join(Dir, "mix.exs"),
                read_fixture("local_mix.exs")
            ),
            ?assertException(throw, rebar_abort, exerl_mix_metadata:generate(Dir)),
            ?assertEqual(Project, 'Elixir.Mix.ProjectStack':peek()),
            ?assertEqual(dev, 'Elixir.Mix':env()),
            ?assertEqual([], 'Elixir.Code':get_compiler_option(no_warn_undefined)),
            ?assertEqual({ok, Cwd}, file:get_cwd()),
            ?assertEqual(false, code:is_loaded('Elixir.MetadataFixture.MixProject'))
        after
            'Elixir.Mix':env(Env),
            'Elixir.Code':put_compiler_option(no_warn_undefined, Undefined)
        end
    end).

prepares_git_dependency(Dir) ->
    Git = os:find_executable("git"),
    _ = exerl_exec:run([Git, "init", "--quiet"], Dir),
    _ = exerl_exec:run([Git, "add", "mix.exs"], Dir),
    _ = exerl_exec:run(
        [
            Git,
            "-c",
            "user.name=Test",
            "-c",
            "user.email=test@example.invalid",
            "-c",
            "commit.gpgsign=false",
            "commit",
            "--quiet",
            "-m",
            "Fixture"
        ],
        Dir
    ),
    State = rebar_state:new(),
    {ok, AppInfo0} = rebar_app_info:new(metadata_fixture, "0.1.0", Dir),
    AppInfo = rebar_app_info:source(AppInfo0, {git, "file://localhost" ++ Dir, {ref, "HEAD"}}),
    {ok, Resource} = exerl_mix_resource:init(git, State),
    ResourceState = rebar_resource_v2:find_resource_state(git, [Resource]),
    Target = Dir ++ "_clone",
    try
        ok = exerl_mix_resource:download(Target, AppInfo, State, ResourceState),
        {ok, [{deps, Deps}]} = file:consult(filename:join(Target, "rebar.config")),
        ?assertEqual([{runtime_app, "~> 1.0", {pkg, "runtime_package"}}], Deps)
    after
        case filelib:is_dir(Target) of
            true -> ok = file:del_dir_r(Target);
            false -> ok
        end
    end.
