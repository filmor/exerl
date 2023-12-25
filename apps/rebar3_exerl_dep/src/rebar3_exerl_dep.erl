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
-define(RES, elixir).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_resource(State, {?RES, ?MODULE}),
    {ok, State1}.

-spec init(atom(), rebar_state:t()) -> {ok, rebar_resource_v2:resource()}.
init(Type, _State) ->
    Resource = rebar_resource_v2:new(Type, ?MODULE, #{}),
    {ok, Resource}.

lock(AppInfo, _) ->
    case rebar_app_info:source(AppInfo) of
        {?RES, Version} ->
            Name = rebar_app_info:name(AppInfo),
            Version1 = find_matching(Version),
            {?RES, Name, Version1};
        {?RES, Name, Version} ->
            Version1 = find_matching(Version),
            {?RES, Name, Version1}
    end.

needs_update(_AppInfo, _) ->
    % TODO: Take floating version into account
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
    % AppOpts = rebar_app_info:opts(AppInfo),
    % AppOpts1 = rebar_dir:src_dirs(AppOpts, []),

    {?RES, Name, {tag, Tag}} = lock(AppInfo, State),
    rebar_log:log(debug, "Ensuring that tag ~s is cached", [Tag]),

    Path = exerl_r3:ensure_pkg(State, Tag),
    rebar_log:log(debug, "Downloaded precompiled Elixir to ~s", [Path]),
    extract_lib_from_pkg(Path, Name, TmpDir),

    ok.

make_vsn(_Param, _State) ->
    {error, "Replacing version of type elixir is not supported"}.

extract_lib_from_pkg(Filename, App, Dest) ->
    Prefix = lists:flatten(["lib/", binary_to_list(App), "/"]),
    PrefixLen = length(Prefix),

    {ok, _} = zip:foldl(
        fun(Name, _GetInfo, GetBin, Acc) ->
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

find_matching({tag, Tag}) ->
    {tag, list_to_binary([Tag])};
find_matching(Requirement) ->
    {ok, Req0} = verl:parse_requirement(list_to_binary("~> " ++ Requirement)),
    Req1 = verl:compile_requirement(Req0),
    rebar_log:log(debug, "Trying to find release from requirement ~s", [Requirement]),
    % TODO: Handle fully defined version? Cache release info?
    Releases = [
        Release
     || Release <- exerl_pkg:get_releases(),
        verl:is_match(exerl_pkg:version(Release), Req1)
    ],

    [BestMatch | _] = lists:sort(
        fun(Lhs, Rhs) ->
            verl:gt(exerl_pkg:version(Lhs), exerl_pkg:version(Rhs))
        end,
        Releases
    ),

    {tag, exerl_pkg:tag(BestMatch)}.
