-module(exerl_find).

-export([
    from_executable/0
]).

from_executable() ->
    Ex =
        case os:find_executable("elixir") of
            undefined ->
                error(not_found);
            V ->
                V
        end,
    Args = [
        Ex,

        "-e",

        "Application.app_dir(:elixir)\n"
        "|> Path.dirname\n"
        "|> Path.expand\n"
        "|> IO.puts"
    ],

    list_to_binary(exerl_exec:run(Args)).
