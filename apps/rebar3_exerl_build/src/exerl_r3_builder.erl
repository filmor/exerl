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

    CurrentPwd = file:get_cwd(),
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
        ?Task:run(<<"loadpaths">>, [
            <<"--no-deps-check">>,
            <<"--no-archives-check">>
        ]),
        rebar_api:info("Loaded paths", []),

        rebar_api:info("Config: ~p", ['Elixir.Mix.Project':config()]),
        rebar_api:info("AppPath: ~p", ['Elixir.Mix.Project':app_path('Elixir.Mix.Project':config())]),

        ?Task:run(<<"compile">>, []),
        rebar_api:info("Compiled", []),
        % code:purge(?Project),
        ok
    after
        file:set_cwd(CurrentPwd)
    end.

load_mix_exs() ->
    Undefined = ?Code:get_compiler_option(no_warn_undefined),
    ?Code:put_compiler_option(no_warn_undefined, all),
    ?Code:compile_file(<<"mix.exs">>),
    ?Code:put_compiler_option(no_warn_undefined, Undefined),
    ok.
