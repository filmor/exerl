-module(exerl_mix_converger).

-define(Dep, 'Elixir.Mix.Dep').
-define(Conv, 'Elixir.Mix.RemoteConverger').

-behaviour(?Conv).

-export([
    register/0,

    'remote?'/1,
    converge/2,
    deps/2,
    post_converge/0
]).

register() ->
    rebar_api:debug("Registering remote converger", []),
    ?Conv:register(?MODULE).

'remote?'(_Dep) ->
    rebar_api:debug("remote?: ~p", [_Dep]),
    true.

converge(_Deps, Map) ->
    rebar_api:debug("converge: ~p ~p", [_Deps, Map]),
    Map.

deps(_Dep, Map) ->
    rebar_api:debug("deps: ~p ~p", [_Dep, Map]),
    [].

post_converge() ->
    ok.
