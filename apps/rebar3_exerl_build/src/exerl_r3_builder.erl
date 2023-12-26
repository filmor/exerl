-module(exerl_r3_builder).

-export([build/1]).

-define(Project, 'Elixir.Mix.Project').
-define(Task, 'Elixir.Mix.Task').
-define(Code, 'Elixir.Code').
-define(Dep, 'Elixir.Mix.Dep').
-define(Mix, 'Elixir.Mix').
-define(Hex, 'Elixir.Hex').
-define(Conv, 'Elixir.Mix.Dep.Converger').

build(AppInfo) ->
    exerl_util:ensure_started(logger),
    exerl_util:ensure_started(mix),
    exerl_util:ensure_started(eex),
    exerl_util:ensure_started(ex_unit),

    rebar_api:debug("Mode: ~p", [code:get_mode()]),

    [code:ensure_loaded(list_to_atom(filename:rootname(filename:basename(F))))
 || P <- code:get_path(), F <- filelib:wildcard(P ++ "/*.beam")],

    {ok, CurrentPwd} = file:get_cwd(),
    NewCwd = rebar_app_info:dir(AppInfo),
    rebar_api:debug("Switching cwd to ~p...", [NewCwd]),
    ok = file:set_cwd(NewCwd),
    try
        rebar_api:debug("Ensuring hex is installed...", []),
        ?Task:run(<<"local.hex">>, [<<"--force">>, <<"--if-missing">>]),

        rebar_api:debug("Loading mix.exs...", []),

        load_mix_exs(),
        rebar_api:debug("Loading config...", []),
        ?Task:run(<<"loadconfig">>),

        rebar_api:debug("Loading dependencies...", []),
        Deps = ?Dep:load_and_cache(),
        rebar_api:debug("Deps: ~p", [Deps]),

        ?Task:run(<<"loadpaths">>, [<<"--no-archives-check">>]),

        rebar_api:debug("Config: ~p", [?Project:config()]),
        rebar_api:debug("AppPath: ~p", [?Project:app_path(?Project:config())]),
        rebar_api:debug("CodePath: ~p", [?Project:compile_path(?Project:config())]),

        rebar_api:debug("ModInfo: ~p", ['Elixir.EEx':module_info()]),
        rebar_api:debug("Path: ~p", [code:get_path()]),
        ?Task:run(<<"compile">>, []),
        rebar_api:debug("Compiled", []),
        code:purge(?Project),
        ok
    after
        file:set_cwd(CurrentPwd),
        ok
    end.

load_mix_exs() ->
    Undefined = ?Code:get_compiler_option(no_warn_undefined),
    ?Code:put_compiler_option(no_warn_undefined, all),
    ?Code:compile_file(<<"mix.exs">>),
    ?Code:put_compiler_option(no_warn_undefined, Undefined),
    ok.
