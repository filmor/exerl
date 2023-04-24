-module(exerl_r3_builder).

-export([build/1]).

-define(Project, 'Elixir.Mix.Project').
-define(Task, 'Elixir.Mix.Task').
-define(Code, 'Elixir.Code').
-define(Dep, 'Elixir.Mix.Dep').
-define(Mix, 'Elixir.Mix').
-define(Conv, 'Elixir.Mix.Dep.Converger').

build(AppInfo) ->
    exerl_util:ensure_started(logger),
    exerl_util:ensure_started(mix),

    CurrentPwd = file:get_cwd(),
    NewCwd = rebar_app_info:dir(AppInfo),
    rebar_api:debug("Switching cwd to ~p...", [NewCwd]),
    ok = file:set_cwd(NewCwd),
    try
        % meck:new('Elixir.Mix.Dep', [passthrough]),
        % meck:expect('Elixir.Mix.Dep', check_lock, 1, fun(Dep) ->
        %     Dep#{ status => {ok, []} }
        % end),
        application:ensure_all_started(meck),

        catch meck:new('Elixir.Mix.Dep.Loader', [passthrough]),
        meck:expect('Elixir.Mix.Dep.Loader', children, 0, fun() -> [] end),

        catch meck:new('Elixir.Mix.Utils', [passthrough]),
        meck:expect('Elixir.Mix.Utils', symlink_or_copy, 3, fun
            (_, A, A) ->
                ok;
            (A, B, C) ->
                meck:passthrough([A, B, C])
        end),

        rebar_api:debug("Loading mix.exs...", []),

        load_mix_exs(),
        rebar_api:debug("Loading config...", []),
        ?Task:run(<<"loadconfig">>),

        rebar_api:debug("Loading dependencies...", []),
        Env = ?Mix:env(),
        Target = ?Mix:target(),
        ?Conv:converge([], [{env, Env}, {target, Target}], [], fun(A, B, C) ->
            rebar_api:debug("Converge callback ~p ~p ~p", [A, B, C])
        end),
        ?Dep:load_and_cache(),

        ?Task:run(<<"loadpaths">>, [<<"--no-archives-check">>]),
        rebar_api:info("Loaded paths", []),

        rebar_api:info("Config: ~p", ['Elixir.Mix.Project':config()]),
        rebar_api:info("AppPath: ~p", ['Elixir.Mix.Project':app_path('Elixir.Mix.Project':config())]),

        ?Task:run(<<"compile">>, []),
        rebar_api:info("Compiled", []),
        % code:purge(?Project),
        ok
    after
        application:stop(meck),
        file:set_cwd(CurrentPwd)
    end.

load_mix_exs() ->
    Undefined = ?Code:get_compiler_option(no_warn_undefined),
    ?Code:put_compiler_option(no_warn_undefined, all),
    ?Code:compile_file(<<"mix.exs">>),
    ?Code:put_compiler_option(no_warn_undefined, Undefined),
    ok.

build_lock() ->
    ok.
