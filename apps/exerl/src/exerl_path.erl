-module(exerl_path).

-export([
    data_path/0,
    elixir_path/1
]).

data_path() ->
    filename:basedir(user_data, "exerl").

elixir_path(Version) ->
    filename:join([
        data_path(),
        "elixir",
        string:join(
            [integer_to_list(I) || I <- Version],
            "."
        )
    ]).

% Functions for
% - set/get elixir path
% - get proper cache directory (~/.cache/exerl)
