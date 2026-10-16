-module(exerl_SUITE).

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").

-export([suite/0, all/0, init_per_suite/1, end_per_suite/1]).
-export([
    compiled_sources/1,
    mix_dependencies/1,
    consolidated_protocols/1,
    stale_dependency_protocol/1,
    release/1
]).

-type config() :: [{atom(), term()}].

-doc "Exercise the bundled consumer project using the local plugin in a separate VM.".
-spec suite() -> [tuple()].
suite() ->
    [{timetrap, {minutes, 15}}].

-spec all() -> [atom()].
all() ->
    [
        compiled_sources,
        mix_dependencies,
        consolidated_protocols,
        stale_dependency_protocol,
        release
    ].

-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    Source = proplists:get_value(source, ?MODULE:module_info(compile)),
    Root = filename:dirname(filename:dirname(Source)),
    Project = filename:join(?config(priv_dir, Config), "exerl_test"),
    Fixture = filename:join(?config(data_dir, Config), "exerl_test"),
    filelib:fold_files(
        Fixture,
        ".*",
        true,
        fun(Path, ok) ->
            Relative = lists:nthtail(length(filename:split(Fixture)), filename:split(Path)),
            Target = filename:join([Project | Relative]),
            ok = filelib:ensure_dir(Target),
            {ok, _} = file:copy(Path, Target),
            ok
        end,
        ok
    ),
    Checkout = filename:join([Project, "_checkouts", "exerl"]),
    ok = filelib:ensure_dir(Checkout),
    ok = file:make_symlink(Root, Checkout),
    {ok, _} = file:copy(
        filename:join(Root, "rebar.config.script"),
        filename:join(Project, "rebar.config.script")
    ),
    select_elixir(Project),
    Rebar = os:find_executable("rebar3"),
    run(Rebar, ["compile"], Project),
    [{project, Project}, {rebar, Rebar} | Config].

-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
    ok.

-doc "Compile all top-level and nested Elixir modules alongside Erlang sources.".
-spec compiled_sources(config()) -> ok.
compiled_sources(Config) ->
    eval(
        Config,
        lib_dir(Config),
        version_check() ++
            "Modules = [\"Elixir.\" ++ Prefix ++ Name"
            " || Prefix <- [\"\", \"Test2.\", \"Test3.\"],"
            " Name <- [\"Bla\" | [\"Bla\" ++ integer_to_list(N) || N <- lists:seq(1, 9)]]],"
            "lists:foreach(fun(Name) -> 1 = (list_to_atom(Name)):a() end, Modules),"
            "test4 = test4:module_info(module),"
            "ok = application:set_env(tzdata, autoupdate, disabled),"
            "{ok, _} = application:ensure_all_started(exerl_test),"
            "[_ | _] = blubb:aliases(),"
            "halt(0)."
    ).

-doc "Build both Rebar and Mix dependencies and generate loadable OTP applications.".
-spec mix_dependencies(config()) -> ok.
mix_dependencies(Config) ->
    eval(
        Config,
        lib_dir(Config),
        "lists:foreach(fun(App) -> ok = application:load(App) end,"
        " [hackney, tzdata, easter, phoenix, bandit, recon, explorer, protobuf]),"
        "lists:foreach(fun(Module) -> {module, Module} = code:ensure_loaded(Module) end,"
        " [hackney, 'Elixir.Tzdata', 'Elixir.Easter', 'Elixir.Phoenix',"
        " 'Elixir.Bandit', recon, 'Elixir.Explorer', 'Elixir.Protobuf']),"
        "halt(0)."
    ).

-doc "The post-compile hook emits consolidated protocols into the consumer application.".
-spec consolidated_protocols(config()) -> ok.
consolidated_protocols(Config) ->
    Ebin = filename:join([lib_dir(Config), "exerl_test", "ebin"]),
    eval(
        Config,
        lib_dir(Config),
        lists:flatten(
            io_lib:format(
                "lists:foreach(fun(Protocol) ->"
                " {module, Protocol} = code:load_abs(filename:join(~tp, atom_to_list(Protocol))),"
                " true = 'Elixir.Protocol':'consolidated?'(Protocol)"
                " end, ~tp), halt(0).",
                [Ebin, protocols(Config)]
            )
        )
    ).

-doc "Remove consolidated copies when a dependency changes a protocol into a regular module.".
-spec stale_dependency_protocol(config()) -> ok.
stale_dependency_protocol(Config) ->
    Ebin = filename:join([lib_dir(Config), "exerl_test", "ebin"]),
    Temp = filename:join(?config(priv_dir, Config), "old_protocol"),
    ok = filelib:ensure_dir(filename:join(Temp, "placeholder")),
    eval(
        Config,
        lib_dir(Config),
        lists:flatten(
            io_lib:format(
                "{ok, _} = application:ensure_all_started(elixir),"
                "Original = code:which('Elixir.Easter'),"
                "{ok, {_, [{compile_info, Info}]}} = beam_lib:chunks(Original, [compile_info]),"
                "{source, Source} = lists:keyfind(source, 1, Info),"
                "[{Module, Beam}] = 'Elixir.Code':compile_string("
                " ~w, list_to_binary(Source)),"
                "Name = atom_to_list(Module) ++ \".beam\","
                "ok = file:write_file(filename:join(~tp, Name), Beam),"
                "true = code:add_patha(~tp),"
                "true = code:soft_purge(Module),"
                "{module, Module} = code:load_abs(filename:join(~tp, atom_to_list(Module))),"
                "{ok, Consolidated} = 'Elixir.Protocol':consolidate(Module, []),"
                "ok = file:write_file(filename:join(~tp, Name), Consolidated), halt(0).",
                [
                    ~"defprotocol Easter do\n  def stale(value)\nend\n",
                    Temp,
                    Temp,
                    Temp,
                    Ebin
                ]
            )
        )
    ),
    run(?config(rebar, Config), ["consolidate_protocols"], ?config(project, Config)),
    ?assertNot(filelib:is_regular(filename:join(Ebin, "Elixir.Easter.beam"))),
    eval(
        Config,
        lib_dir(Config),
        "{module, 'Elixir.Easter'} = code:ensure_loaded('Elixir.Easter'),"
        "false = erlang:function_exported('Elixir.Easter', '__protocol__', 1), halt(0)."
    ).

-doc "Assemble a release retaining consolidated protocols but excluding their originals.".
-spec release(config()) -> ok.
release(Config) ->
    Project = ?config(project, Config),
    run(?config(rebar, Config), ["release"], Project),
    Release = filename:join([Project, "_build", "default", "rel", "exerl_test"]),
    ?assert(filelib:is_regular(filename:join([Release, "bin", "exerl_test"]))),
    {ok, [{release, _, _, Apps}]} = file:consult(
        filename:join([Release, "releases", "0.0.1", "exerl_test.rel"])
    ),
    lists:foreach(
        fun(App) -> ?assert(lists:keymember(App, 1, Apps)) end,
        [exerl_test, tzdata, easter, elixir]
    ),
    ReleaseLib = filename:join(Release, "lib"),
    lists:foreach(
        fun(Protocol) ->
            Beam = atom_to_list(Protocol) ++ ".beam",
            ?assertMatch(
                [_],
                filelib:wildcard(
                    filename:join([ReleaseLib, "exerl_test-*", "ebin", Beam])
                )
            )
        end,
        protocols(Config)
    ),
    BinDir = filename:join([code:root_dir(), "erts-" ++ erlang:system_info(version), "bin"]),
    Expression = lists:flatten(
        io_lib:format(
            "1 = 'Elixir.Bla':a(), [_ | _] = blubb:aliases(),"
            "{ok, Originals} = application:get_key(elixir, modules),"
            "{ok, Consolidated} = application:get_key(exerl_test, modules),"
            "lists:foreach(fun(Protocol) ->"
            " false = lists:member(Protocol, Originals),"
            " true = lists:member(Protocol, Consolidated),"
            " true = 'Elixir.Protocol':'consolidated?'(Protocol)"
            " end, ~tp), halt(0).",
            [protocols(Config)]
        )
    ),
    run(
        filename:join(BinDir, "erlexec"),
        [
            "-noshell",
            "-mode",
            "embedded",
            "-boot",
            filename:join([Release, "releases", "0.0.1", "start"]),
            "-tzdata",
            "autoupdate",
            "disabled",
            "-eval",
            Expression
        ],
        Project,
        [{"ROOTDIR", Release}, {"BINDIR", BinDir}, {"EMU", "beam"}, {"PROGNAME", "erl"}]
    ).

lib_dir(Config) ->
    filename:join([?config(project, Config), "_build", "default", "lib"]).

select_elixir(Project) ->
    case os:getenv("EXERL_TEST_ELIXIR") of
        false ->
            ok;
        _ ->
            LockFile = filename:join(Project, "rebar.lock"),
            {ok, [{Format, Locks} | Metadata]} = file:consult(LockFile),
            Selected = [
                Lock
             || Lock = {_, Source, _} <- Locks,
                element(1, Source) =/= ex
            ],
            ok = file:write_file(
                LockFile,
                [io_lib:format("~p.~n", [Term]) || Term <- [{Format, Selected} | Metadata]]
            )
    end.

version_check() ->
    case os:getenv("EXERL_TEST_ELIXIR") of
        false ->
            "";
        Version ->
            lists:flatten(
                io_lib:format("~w = 'Elixir.System':version(),", [list_to_binary(Version)])
            )
    end.

protocols(Config) ->
    [
        'Elixir.String.Chars',
        'Elixir.List.Chars',
        'Elixir.Inspect',
        'Elixir.Enumerable',
        'Elixir.Collectable'
    ] ++
        case
            filelib:is_regular(
                filename:join([lib_dir(Config), "elixir", "ebin", "Elixir.JSON.Encoder.beam"])
            )
        of
            true -> ['Elixir.JSON.Encoder'];
            false -> []
        end.

eval(Config, LibDir, Expression) ->
    Paths = filelib:wildcard(filename:join([LibDir, "*", "ebin"])),
    run(
        os:find_executable("erl"),
        ["-noshell", "-pa" | Paths] ++ ["-eval", Expression],
        ?config(project, Config)
    ).

run(Executable, Args, Dir) ->
    run(Executable, Args, Dir, []).

run(Executable, Args, Dir, Env) ->
    ct:pal("Running ~ts ~p in ~ts", [Executable, Args, Dir]),
    Port = open_port(
        {spawn_executable, Executable},
        [
            binary,
            exit_status,
            stderr_to_stdout,
            use_stdio,
            {args, Args},
            {cd, Dir},
            {env, [{"REBAR_BASE_DIR", false} | Env]}
        ]
    ),
    try
        collect_output(Port, [])
    after
        case erlang:port_info(Port) of
            undefined -> ok;
            _ -> port_close(Port)
        end
    end.

collect_output(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            collect_output(Port, [Data | Acc]);
        {Port, {exit_status, Status}} ->
            Output = iolist_to_binary(lists:reverse(Acc)),
            ct:pal("~ts", [Output]),
            case Status of
                0 -> ok;
                _ -> ct:fail({command_failed, Status})
            end
    end.
