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

-define(D(Fmt, Args), rebar_log:log(diagnostic, Fmt, Args)).

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
    ?D("Source: ~p, OutDir: ~p", [Source, OutDir]),
    {ok, Modules, _Warnings} = 'Elixir.Kernel.ParallelCompiler':compile_to_path(
        [list_to_binary(Source)],
        list_to_binary(OutDir)
    ),
    % Write list of modules to file
    % file:write_file(

    ok.

clean(Files, AppInfo) ->
    OutDir = rebar_app_info:out_dir(AppInfo),

    FilesSet = sets:from_list(Files, [{version, 2}]),

    ToDelete =
        lists:filtermap(
            fun(Beam) ->
                rebar_log:log(diagnostic, "Beam: ~s", [Beam]),
                case beam_lib:chunks(Beam, [compile_info]) of
                    {ok, {Mod, [{compile_info, CompileInfo}]}} ->
                        case proplists:get_value(source, CompileInfo) of
                            Source when is_map_key(Source, FilesSet) ->
                                ?D("Beam file ~s was compiled from ~s, deleting", [
                                    Beam,
                                    Source
                                ]),
                                {true, Beam};
                            _Else ->
                                false
                        end;
                    _Else ->
                        false
                end
            end,
            filelib:wildcard(filename:join([OutDir, "ebin", "*.beam"]))
        ),
    rebar_file_utils:delete_each(ToDelete),
    ok.

lock_file_name(AppInfo) ->
    rebar_app_info:dir(AppInfo),
    ok.
