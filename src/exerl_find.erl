-module(exerl_find).

-export([
    from_executable/0,
    set_code_path/0
]).

-spec set_code_path() -> ok | already_added.
set_code_path() ->
    case code:which(elixir) of
        non_existing ->
            Path = exerl_find:from_executable(),
            Paths = filelib:wildcard(
                binary_to_list(
                    filename:join([Path, <<"*">>, <<"ebin">>])
                )
            ),
            code:add_pathsz(Paths),
            ok;
        _ ->
            already_added
    end.

-spec from_executable() -> binary().
from_executable() ->
    Ex =
        case os:find_executable("elixir") of
            false ->
                error(not_found);
            V ->
                V
        end,

    Args = [
        Ex,

        "-e",

        "Application.app_dir(:elixir)"
        " |> Path.dirname"
        " |> Path.expand"
        " |> IO.puts"
    ],

    list_to_binary(exerl_exec:run(Args)).
