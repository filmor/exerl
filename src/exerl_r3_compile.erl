-module(exerl_r3_compile).

-behaviour(rebar_compiler).

-export([
    context/1,
    needed_files/4,
    dependencies/3,
    compile/4,
    clean/2
]).

-include_lib("providers/include/providers.hrl").

context(AppInfo) ->
    EbinDir = rebar_app_info:ebin_dir(AppInfo),
    Mappings = [{".beam", EbinDir}],

    OutDir = rebar_app_info:dir(AppInfo),
    SrcDirs = rebar_dir:src_dirs(rebar_app_info:opts(AppInfo), ["src"]),
    ExistingSrcDirs = lists:filter(
        fun(D) ->
            ec_file:is_dir(filename:join(OutDir, D))
        end,
        SrcDirs
    ),

    _RebarOpts = rebar_app_info:opts(AppInfo),

    #{
        src_dirs => ExistingSrcDirs,
        src_ext => ".ex",
        include_dirs => [],
        out_mappings => Mappings,
        dependencies_opts => []
    }.

needed_files(_Graph, FoundFiles, _, _AppInfo) ->
    {{[], []}, {{[], FoundFiles}, []}}.

dependencies(Source, SourceDir, Dirs) ->
    [].

compile(Source, [{_, OutDir}], Config, ErlOpts) ->
    rebar_log:log(diagnostic, "Source: ~p, OutDir: ~p", [Source, OutDir]),
    {ok, Modules, _Warnings} = 'Elixir.Kernel.ParallelCompiler':compile_to_path(
        [list_to_binary(Source)],
        list_to_binary(OutDir)
    ),
    ok.

clean(Files, AppInfo) ->
    % TODO
    ok.
