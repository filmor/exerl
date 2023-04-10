-module(exerl_r3_resource).

-behaviour(rebar_resource_v2).

-export([
    init/2,
    lock/2,
    download/4,
    needs_update/2,
    make_vsn/2
]).

-spec init(atom(), rebar_state:t()) -> {ok, rebar_resource_v2:resource()}.
init(Type, _State) ->
    Resource = rebar_resource_v2:new(Type, ?MODULE, #{}),
    {ok, Resource}.

lock(AppInfo, _) ->
    rebar_app_info:source(AppInfo).

needs_update(_AppInfo, _) ->
    error(needs_update),
    false.

download(TmpDir, AppInfo, State, _MyState) ->
    Name = rebar_app_info:name(AppInfo),
    {elixir, Version0} = rebar_app_info:source(AppInfo),
    Version = [list_to_integer(V) || V <- string:split(Version0, ".", all)],
    Path = exerl_path:elixir_path(Version),

    exerl_download:download_and_extract(Version),

    Path1 = filename:join([Path, lib, Name]),

    lists:foreach(
      fun (P) ->
            rebar_api:info("Copying from ~s...", [P]),
              ec_file:copy(P, TmpDir)
      end,
    filelib:wildcard(filename:join(Path1, "*"))
     ),

    ok.

make_vsn(Param, State) ->
    rebar_api:info("~p~n~p", [Param, State]),
    {plain, "1.0.0"}.
