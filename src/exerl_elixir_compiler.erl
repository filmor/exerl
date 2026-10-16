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

-type out_mappings() :: [{string(), file:filename()}].
-type compilation_result() ::
    {ok, [{file:filename(), file:filename(), map()}], [string()]}
    | {error, [string()], [string()]}.

-spec context(rebar_app_info:t()) -> map().
context(AppInfo) ->
    try
        exerl_util:ensure_elixir(),
        exerl_util:ensure_started(mix),

        % elp:ignore W0023 (atoms_exhaustion)
        AppName = binary_to_atom(rebar_app_info:name(AppInfo)),
        ?ProjectStack:push(
            AppName,
            [{app, AppName}],
            <<"nofile">>
        ),

        EbinDir = exerl_util:ensure_string(rebar_app_info:ebin_dir(AppInfo)),
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

-spec needed_files(digraph:graph(), [file:filename()], out_mappings(), rebar_app_info:t()) ->
    {{[], []}, {{[], [file:filename()]}, []}}.
needed_files(_Graph, FoundFiles, _, _AppInfo) ->
    {{[], []}, {{[], FoundFiles}, []}}.

-spec dependencies(file:filename(), file:filename(), [file:filename()]) -> [].
dependencies(_Source, _SourceDir, _Dirs) ->
    [].

-spec compile_and_track(file:filename(), out_mappings(), dict:dict(), list()) ->
    compilation_result().
compile_and_track(Source, [{_, OutDir}], _Config, _Opts) ->
    case
        ?Compiler:compile_to_path(
            [list_to_binary(Source)],
            list_to_binary(OutDir),
            [{return_diagnostics, true}]
        )
    of
        {ok, Modules, Info} ->
            rebar_api:debug("[exerl] Compiled ~p from ~s", [Modules, Source]),
            ModuleToPath = fun(Module) ->
                filename:join(OutDir, atom_to_list(Module) ++ ".beam")
            end,
            {ok, [{Source, ModuleToPath(Module), #{}} || Module <- Modules], warnings(Info)};
        {error, Errors, Info} ->
            {error, [format_diagnostic(Error) || Error <- Errors], warnings(Info)}
    end.

-spec compile(file:filename(), out_mappings(), dict:dict(), list()) -> no_return().
compile(_, _, _, _) ->
    % Only added because the compiler behaviour expects it
    error(not_implemented).

-spec clean([file:filename()], rebar_app_info:t()) -> ok.
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

warnings(#{compile_warnings := Compile, runtime_warnings := Runtime}) ->
    [format_diagnostic(Warning) || Warning <- Compile ++ Runtime].

format_diagnostic(#{file := File, position := Position, message := Message}) ->
    unicode:characters_to_list(io_lib:format("~ts:~tp: ~ts", [File, Position, Message])).
