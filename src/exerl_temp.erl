-module(exerl_temp).

-export([
    mkdtemp/0
]).

-spec mkdtemp() -> file:name() | {error, term()}.
mkdtemp() ->
    UniqueNumber = erlang:integer_to_list(rand:uniform(1000000000000)),
    TmpDirPath = filename:join(tmp(), [".tmp_dir-", UniqueNumber]),
    case mkdir_p(TmpDirPath) of
        ok ->
            TmpDirPath;
        Error ->
            Error
    end.

-spec tmp() -> file:name().
tmp() ->
    case erlang:system_info(system_architecture) of
        "win32" ->
            case os:getenv("TEMP") of
                false ->
                    "./tmp";
                Val ->
                    Val
            end;
        _SysArch ->
            case os:getenv("TMPDIR") of
                false ->
                    "/tmp";
                Val ->
                    Val
            end
    end.

%% @doc Makes a directory including parent dirs if they are missing.
-spec mkdir_p(string()) -> ok | {error, Reason :: file:posix()}.
mkdir_p(Path) ->
    %% We are exploiting a feature of ensuredir that that creates all
    %% directories up to the last element in the filename, then ignores
    %% that last element. This way we ensure that the dir is created
    %% and not have any worries about path names
    DirName = filename:join([filename:absname(Path), "tmp"]),
    filelib:ensure_dir(DirName).
