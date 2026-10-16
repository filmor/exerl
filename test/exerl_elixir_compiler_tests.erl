-module(exerl_elixir_compiler_tests).

-include_lib("eunit/include/eunit.hrl").

-spec erlang_only_context_test() -> ok.
erlang_only_context_test() ->
    Source = proplists:get_value(source, exerl:module_info(compile)),
    Root = filename:dirname(filename:dirname(Source)),
    {ok, AppInfo0} = rebar_app_info:new(exerl, "0.1.0", Root),
    Ebin = filename:dirname(code:which(exerl)),
    AppInfo = rebar_app_info:ebin_dir(AppInfo0, Ebin),
    Applications = lists:sort(application:which_applications()),
    Paths = code:get_path(),
    #{src_ext := ".ex"} = exerl_elixir_compiler:context(AppInfo),
    ?assertEqual(Applications, lists:sort(application:which_applications())),
    ?assertEqual(Paths, code:get_path()).
