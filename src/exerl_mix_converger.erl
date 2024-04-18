-module(exerl_mix_converger).

-include("exerl.hrl").

-define(LOG(Func, Args), begin
    Res = erlang:apply(Func, Args),
    rebar_api:debug("[~s]~n~p~n=>~n~p", [
        ??Func, Args, Res
    ]),
    Res
end).

% -behaviour(?Conv).

-export([
    register/0,
    unregister/0,

    'remote?'/1,
    converge/2,
    deps/2,
    post_converge/0
]).

register() ->
    ?Conv:register(?MODULE).

unregister() ->
    ?Conv:register(nil).

'remote?'(Dep) ->
    ?LOG(fun ?HexConv:'remote?'/1, [Dep]).

converge(Deps, Map) ->
    ?LOG(fun ?HexConv:converge/2, [Deps, Map]).

deps(Dep, Map) ->
    ?LOG(fun ?HexConv:deps/2, [Dep, Map]).

post_converge() ->
    % ?LOG(post_converge, ?HexConv:post_converge()).
    % Skip original post_converge
    ok.
