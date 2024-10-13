-module(exerl_elixir_compiler).

-behaviour(rebar_compiler).

-include("exerl.hrl").

-export([
    context/1,
    needed_files/4,
    dependencies/3,
    compile/4,
    compile_and_track/4,
    clean/2
]).

% -include_lib("providers/include/providers.hrl").

context(AppInfo) ->
    try
        exerl_util:ensure_elixir(),
        exerl_util:ensure_started(mix),

        AppName = binary_to_atom(rebar_app_info:name(AppInfo)),
        ?ProjectStack:push(
            AppName,
            [{app, AppName}],
            <<"nofile">>
        ),

        EbinDir = rebar_app_info:ebin_dir(AppInfo),
        Mappings = [{".beam", EbinDir}],

        OutDir = rebar_app_info:dir(AppInfo),
        SrcDirs = rebar_dir:src_dirs(rebar_app_info:opts(AppInfo), ["src"]),
        ExistingSrcDirs = lists:filter(
            fun(D) -> ec_file:is_dir(filename:join(OutDir, D)) end,
            SrcDirs
        ),

        _RebarOpts = rebar_app_info:opts(AppInfo),

        % Ensure that the Elixir compiler does not load the previously built
        % binaries and complains about it
        code:del_path(EbinDir),

        #{
            src_dirs => ExistingSrcDirs,
            src_ext => ".ex",
            include_dirs => [],
            out_mappings => Mappings,
            dependencies_opts => []
        }
    catch
        Error:Reason:St ->
            rebar_api:debug("[exerl] ~s", [erl_error:format_exception(Error, Reason, St)]),
            error(Reason)
    end.

needed_files(_Graph, FoundFiles, _, _AppInfo) ->
    {{[], []}, {{[], FoundFiles}, []}}.

dependencies(_Source, _SourceDir, _Dirs) ->
    [].

compile_and_track(Source, [{_, OutDir}], _Config, _Opts) ->
    case
        ?Compiler:compile_to_path(
            [list_to_binary(Source)],
            list_to_binary(OutDir)
        )
    of
        {ok, Modules, Warnings} ->
            rebar_api:debug("[exerl] Compiled ~p from ~s", [Modules, Source]),
            ModuleToPath = fun(Module) ->
                filename:join(OutDir, atom_to_list(Module) ++ ".beam")
            end,
            {ok, [{Source, ModuleToPath(Module), #{}} || Module <- Modules], Warnings};
        {error, _, _} ->
            rebar_api:debug("[exerl] Failed to compile ~s", [Source]),
            error
    end.

compile(_, _, _, _) ->
    % Only added because the compiler behaviour expects it
    error(not_implemented).

clean(Files, AppInfo) ->
    OutDir = rebar_app_info:ebin_dir(AppInfo),

    FilesSet = sets:from_list(Files, [{version, 2}]),

    ToDelete =
        lists:filtermap(
            fun(Beam) ->
                case beam_lib:chunks(Beam, [compile_info]) of
                    {ok, {Mod, [{compile_info, CompileInfo}]}} ->
                        case proplists:get_value(source, CompileInfo) of
                            Source when is_map_key(Source, FilesSet) ->
                                rebar_api:debug(
                                    "Beam file ~s (~p) was compiled from ~s, deleting", [
                                        Beam,
                                        Mod,
                                        Source
                                    ]
                                ),
                                {true, Beam};
                            _Else ->
                                false
                        end;
                    _Else ->
                        false
                end
            end,
            filelib:wildcard(filename:join([OutDir, "*.beam"]))
        ),
    rebar_file_utils:delete_each(ToDelete),
    ok.
