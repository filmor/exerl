-module(rebar3_exerl_dep).

-export([
    init/1
]).

-export([
    init/2,
    lock/2,
    download/4,
    needs_update/2,
    make_vsn/2
]).

-behaviour(rebar_resource_v2).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_resource(State, {elixir, ?MODULE}),
    {ok, State1}.

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

do_download(TmpDir, AppInfo, State, _MyState) ->
    Name = rebar_app_info:name(AppInfo),
    % AppOpts = rebar_app_info:opts(AppInfo),
    % AppOpts1 = rebar_dir:src_dirs(AppOpts, []),

    {elixir, Version0} = rebar_app_info:source(AppInfo),
    Version = [list_to_integer(V) || V <- string:split(Version0, ".", all)],
    L = length(Version),
    L = 3,

    Path = exerl_r3:ensure_pkg(State, list_to_binary([Version0])),
    rebar_log:log(debug, "Downloaded precompiled Elixir to ~s", [Path]),
    extract_lib_from_pkg(Path, Name, TmpDir),

    ok.

make_vsn(Param, State) ->
    rebar_api:info("~p~n~p", [Param, State]),
    {plain, "1.0.0"}.

extract_lib_from_pkg(Filename, App, Dest) ->
    Prefix = lists:flatten(["lib/", binary_to_list(App), "/"]),
    PrefixLen = length(Prefix),

    {ok, _} = zip:foldl(
      fun (Name, _GetInfo, GetBin, Acc) ->
        case lists:prefix(Prefix, Name) andalso lists:last(Name) =/= $/ of
            true ->
                rebar_log:log(debug, "Found file ~s in zip", [Name]),

                % Unpack
                NameWithoutPrefix = lists:nthtail(PrefixLen, Name),
                Dest1 = filename:join(Dest, NameWithoutPrefix),
                filelib:ensure_dir(Dest1),
                file:write_file(Dest1, GetBin()),
                Acc;
            false ->
                Acc
        end
      end,
      ok,
      binary_to_list(Filename)
     ),

    ok.
