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
    false.

download(TmpDir, AppInfo, State, _) ->
    ok.

make_vsn(_, _) ->
    {plain, "1.0.0"}.
