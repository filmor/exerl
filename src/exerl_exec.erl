-module(exerl_exec).

-export([
    run/1,
    run/2
]).

-export_type([
    output/0
]).

-type output() :: iolist().

-ifdef(TEST).
-define(MAX_LINE_LENGTH, 32).
-else.
-define(MAX_LINE_LENGTH, 16384).
-endif.

run(Command) ->
    run(Command, undefined).

% Code derived from rebar3
-spec run([string()], file:filename_all() | undefined) -> output().
run(Command, Path) ->
    [Exe | Args] = Command,

    Options0 = [
        binary,
        {args, Args},
        exit_status,
        {line, ?MAX_LINE_LENGTH},
        use_stdio,
        hide,
        eof
    ],

    Options1 =
        case Path of
            undefined ->
                Options0;
            _ ->
                [{cd, Path} | Options0]
        end,

    Port = open_port({spawn_executable, Exe}, Options1),

    try
        case loop(Port, []) of
            {ok, Output} ->
                Output;
            {error, {_Rc, _Output} = Err} ->
                error({cmd_error, Err})
        end
    after
        port_close(Port)
    end.

-spec loop(port(), [T | {incomplete, _}]) -> {ok, [T]} | {error, _}.
loop(Port, Acc) ->
    receive
        {Port, {data, {_, Line}}} ->
            loop(Port, handle_output(Line, Acc));
        {Port, eof} ->
            Data = finalize(Acc),
            receive
                {Port, {exit_status, 0}} ->
                    {ok, Data};
                {Port, {exit_status, Rc}} ->
                    {error, {Rc, Data}}
            end
    end.

finalize(Acc) ->
    lists:reverse(Acc).

handle_output("", Acc) ->
    Acc;
handle_output(<<"">>, Acc) ->
    Acc;
handle_output(Line, Acc0) ->
    [Line, Acc0].
