-module(exerl_mix_scm).

-define(Scm, 'Elixir.Mix.SCM').
-define(HexScm, 'Elixir.Hex.SCM').

% -behaviour(?Scm).

-export([
    'fetchable?'/0,
    format/1,
    format_lock/1,
    accepts_options/2,
    'checked_out?'/1,
    checkout/1,
    update/1,
    lock_status/1,
    'equal?'/2,
    managers/1
]).

'fetchable?'() ->
    ?HexScm:'fetchable?'().

format(_Opts) ->
    <<"Hex package (exerl)">>.

format_lock(Opts) ->
    ?HexScm:format_lock(Opts).

accepts_options(Name, Opts) ->
    ?HexScm:accepts_options(Name, Opts).

'checked_out?'(Opts) ->
    ?HexScm:'checked_out?'(Opts).

lock_status(_Opts) ->
    % ?HexScm:lock_status(Opts).
    ok.

'equal?'(Lhs, Rhs) ->
    ?HexScm:'equal?'(Lhs, Rhs).

update(Opts) ->
    ?HexScm:update(Opts).

managers(Opts) ->
    ?HexScm:managers(Opts).

checkout(Opts) ->
    ?HexScm:checkout(Opts).
