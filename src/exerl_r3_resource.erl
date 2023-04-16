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
    try
        do_download(TmpDir, AppInfo, State, _MyState)
    catch
        Cat:Reason:St ->
            E = erl_error:format_exception(Cat, Reason, St),
            rebar_api:error("Error: ~s", [E]),
            E
    end.

do_download(TmpDir, AppInfo, _State, _MyState) ->
    Name = rebar_app_info:name(AppInfo),
    % AppOpts = rebar_app_info:opts(AppInfo),
    % AppOpts1 = rebar_dir:src_dirs(AppOpts, []),

    {elixir, Version0} = rebar_app_info:source(AppInfo),
    Version = [list_to_integer(V) || V <- string:split(Version0, ".", all)],
    Path = exerl_path:elixir_path(Version),

    rebar_api:info("Downloading elixir version ~p", [Version]),
    Err = catch exerl_download:download_and_extract(Version),
    rebar_api:info("Done: ~p", [Err]),

    Path1 = filename:join([Path, lib, Name]),
    rebar_api:info("Downloaded to ~s", [Path1]),

    _Dir = rebar_app_info:dir(AppInfo),
    true = filelib:is_dir(TmpDir),
    true = filelib:is_dir(Path1),

    ec_file:copy(Path1, TmpDir, [recursive]),

    true = filelib:is_dir(filename:join([Path1, "ebin"])),

    ok.

make_vsn(Param, State) ->
    rebar_api:info("~p~n~p", [Param, State]),
    {plain, "1.0.0"}.
