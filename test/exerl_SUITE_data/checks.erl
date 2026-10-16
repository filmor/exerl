[Test, Ebin, Temp, DataDir | ProtocolNames] = init:get_plain_arguments(),
Protocols = [list_to_atom(Name) || Name <- ProtocolNames],
case Test of
    "compiled_sources" ->
        case os:getenv("EXERL_TEST_ELIXIR") of
            false -> ok;
            Version ->
                Expected = list_to_binary(Version),
                Expected = 'Elixir.System':version()
        end,
        Modules = [
            "Elixir." ++ Prefix ++ Name
         || Prefix <- ["", "Test2.", "Test3."],
            Name <- ["Bla" | ["Bla" ++ integer_to_list(N) || N <- lists:seq(1, 9)]]
        ],
        lists:foreach(fun(Name) -> 1 = (list_to_atom(Name)):a() end, Modules),
        test4 = test4:module_info(module),
        ok = application:set_env(tzdata, autoupdate, disabled),
        {ok, _} = application:ensure_all_started(exerl_test),
        [_ | _] = blubb:aliases();
    "mix_dependencies" ->
        lists:foreach(
            fun(App) -> ok = application:load(App) end,
            [hackney, tzdata, easter, phoenix, bandit, recon, explorer, protobuf]
        ),
        lists:foreach(
            fun(Module) -> {module, Module} = code:ensure_loaded(Module) end,
            [
                hackney, 'Elixir.Tzdata', 'Elixir.Easter', 'Elixir.Phoenix',
                'Elixir.Bandit', recon, 'Elixir.Explorer', 'Elixir.Protobuf'
            ]
        );
    "consolidated_protocols" ->
        lists:foreach(
            fun(Protocol) ->
                {module, Protocol} = code:load_abs(filename:join(Ebin, atom_to_list(Protocol))),
                true = 'Elixir.Protocol':'consolidated?'(Protocol)
            end,
            Protocols
        );
    "stale_dependency_protocol" ->
        {ok, _} = application:ensure_all_started(elixir),
        Original = code:which('Elixir.Easter'),
        {ok, {_, [{compile_info, Info}]}} = beam_lib:chunks(Original, [compile_info]),
        {source, Source} = lists:keyfind(source, 1, Info),
        {ok, Code} = file:read_file(filename:join(DataDir, "old_protocol.ex")),
        [{Module, Beam}] = 'Elixir.Code':compile_string(Code, list_to_binary(Source)),
        Name = atom_to_list(Module) ++ ".beam",
        ok = file:write_file(filename:join(Temp, Name), Beam),
        true = code:add_patha(Temp),
        true = code:soft_purge(Module),
        {module, Module} = code:load_abs(filename:join(Temp, atom_to_list(Module))),
        {ok, Consolidated} = 'Elixir.Protocol':consolidate(Module, []),
        ok = file:write_file(filename:join(Ebin, Name), Consolidated);
    "regular_dependency" ->
        {module, 'Elixir.Easter'} = code:ensure_loaded('Elixir.Easter'),
        false = erlang:function_exported('Elixir.Easter', '__protocol__', 1);
    "release" ->
        1 = 'Elixir.Bla':a(),
        [_ | _] = blubb:aliases(),
        {ok, Originals} = application:get_key(elixir, modules),
        {ok, Consolidated} = application:get_key(exerl_test, modules),
        lists:foreach(
            fun(Protocol) ->
                false = lists:member(Protocol, Originals),
                true = lists:member(Protocol, Consolidated),
                true = 'Elixir.Protocol':'consolidated?'(Protocol)
            end,
            Protocols
        )
end,
halt(0).
