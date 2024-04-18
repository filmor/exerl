-module(exerl_mix_builder).

-export([build/1]).

-define(Project, 'Elixir.Mix.Project').
-define(Task, 'Elixir.Mix.Task').
-define(Code, 'Elixir.Code').

-spec build(rebar_app_info:t()) -> ok.
build(AppInfo) ->
    % Elixir's "require" expects the respective modules to be loaded
    % already. To ensure this, we just force-load all modules that we just
    % compiled.
    exerl_util:ensure_elixir(),

    CodePath = code:get_path(),
    LoadedModules = [M || {M, _} <- code:all_loaded()],

    exerl_util:ensure_loaded(rebar_app_info:deps(AppInfo)),

    {ok, CurrentPwd} = file:get_cwd(),
    NewCwd = rebar_app_info:dir(AppInfo),

    rebar_api:debug("Switching cwd to ~p...", [NewCwd]),
    ok = file:set_cwd(NewCwd),
    try
        rebar_api:debug("Ensuring hex is installed...", []),
        ?Task:run(<<"local.hex">>, [<<"--force">>, <<"--if-missing">>]),

        rebar_api:debug("Loading mix.exs...", []),

        load_mix_exs(),

        rebar_api:debug("Loading dependencies...", []),
        ?Task:run(<<"loadpaths">>, [<<"--no-archives-check">>, <<"--no-compile">>]),

        % rebar_api:debug("[exerl_build] Config: ~p", [?Project:config()]),
        rebar_api:debug("[exerl_build] AppPath: ~p", [?Project:app_path(?Project:config())]),
        rebar_api:debug("[exerl_build] CodePath: ~p", [?Project:compile_path(?Project:config())]),

        code:ensure_modules_loaded(['Elixir.Logger']),
        ?Task:run(<<"compile">>, [
            <<"--from-mix-deps-compile">>,
            <<"--no-archives-check">>,
            <<"--no-warnings-as-errors">>,
            <<"--no-code-path-pruning">>,
            <<"--no-protocol-consolidation">>
        ]),

        rebar_api:debug("Compiled", []),
        ok
    after
        NewLoadedModules = [M || {M, _} <- code:all_loaded()],
        Added = NewLoadedModules -- LoadedModules,
        Removed = LoadedModules -- NewLoadedModules,

        rebar_api:debug("Unloading modules ~p", [Added]),
        lists:foreach(fun code:purge/1, Added),

        rebar_api:debug("Reloading modules ~p", [Removed]),
        lists:foreach(fun code:load_module/1, Removed),

        rebar_api:debug("Resetting code path...", []),
        code:set_path(CodePath),
        
        file:set_cwd(CurrentPwd),
        ok
    end.

load_mix_exs() ->
    Undefined = ?Code:get_compiler_option(no_warn_undefined),
    ?Code:put_compiler_option(no_warn_undefined, all),
    ?Code:compile_file(<<"mix.exs">>),
    ?Code:put_compiler_option(no_warn_undefined, Undefined),
    ok.
