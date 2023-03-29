-module(exerl_struct).

-define(S, '__struct__').

-export([
    call/2,
    call/3
]).

call(S, Func) ->
    call(S, Func, []).

call(#{?S := T} = S, Func, Args) ->
    apply(T, Func, [S | Args]).
