-module(exerl).

-export([
    find/0,
    set_code_path/0,
    set_code_path/1,
    start/0,
    start_mix/0,
    compile/2
]).

-export([init/1]).

find() ->
    exerl_find:from_executable().

set_code_path() ->
    set_code_path(find()).

set_code_path(Path) ->
    case code:which(elixir) of
        non_existing ->
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

start() ->
    set_code_path(),
    {ok, _} = application:ensure_all_started(elixir),
    {ok, _} = application:ensure_all_started(logger),
    ok.

start_mix() ->
    start(),
    {ok, _} = application:ensure_all_started(mix),
    ok.

compile(Paths, Dest) ->
    exerl_compile:compile(Paths, Dest).


-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    start(),
    State1 = rebar_state:prepend_compilers(State, [exerl_r3_compile]),
    State2 = rebar_state:add_resource(State1, {elixir, exerl_r3_resource}),
    {ok, State2}.
