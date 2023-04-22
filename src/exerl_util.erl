-module(exerl_util).

-export([
         ensure_started/1
        ]).

ensure_started(App) ->
    case application:ensure_all_started(App) of
        {ok, _} -> ok;
        {error, already_started} -> ok;
        {error, Error} -> error(Error)
    end.
