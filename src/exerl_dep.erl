-module(exerl_dep).

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

% Lock format: version as a string

-behaviour(rebar_resource_v2).
-define(RES, ex).
-define(FULL, elixir_full).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    State1 = rebar_state:add_resource(State, {?RES, ?MODULE}),
    {ok, State1}.

-spec init(atom(), rebar_state:t()) -> {ok, rebar_resource_v2:resource()}.
init(Type, State) ->
    Builds = exerl_dep_builds:new(State),
    Resource = rebar_resource_v2:new(Type, ?MODULE, Builds),
    {ok, Resource}.

lock(AppInfo, Builds) ->
    case rebar_app_info:source(AppInfo) of
        {?RES, Version} ->
            Name = rebar_app_info:name(AppInfo),
            {tag, Version1} = find_matching(Version, Builds),
            {?RES, Name, tag, Version1};
        % {?RES, Name, Version} ->
        %     {tag, Version1} = find_matching(Version),
        %     {?RES, Name, tag, Version1};
        {?RES, _, tag, _} = Lock ->
            Lock
    end.

needs_update(AppInfo, _Releases) ->
    {?RES, _Name, tag, Vsn} = rebar_app_info:source(AppInfo),
    rebar_api:debug("OldVsn: ~p, Vsn: ~p", [rebar_app_info:original_vsn(AppInfo), Vsn]),
    % TODO
    false.

download(TmpDir, AppInfo, State, Builds) ->
    try
        do_download(TmpDir, AppInfo, State, Builds)
    catch
        Cat:Reason:St ->
            E = erl_error:format_exception(Cat, Reason, St),
            rebar_api:error("Error: ~s", [E]),
            E
    end.

do_download(TmpDir, AppInfo, State, Builds) ->
    {?RES, Name, tag, Tag} = lock(AppInfo, Builds),
    rebar_api:debug("Ensuring that tag ~s is cached", [Tag]),

    [Build | _] = [
        B
     || B <- exerl_dep_builds:builds(Builds),
        exerl_dep_build:tag(B) =:= Tag
    ],

    Path = ensure_pkg(State, Build),
    rebar_api:debug("Downloaded precompiled Elixir to ~s", [Path]),

    % elp:ignore W0023 (atoms_exhaustion)
    NameAtom = binary_to_atom(Name),
    RebarConfig = filename:join(TmpDir, "rebar.config"),

    case NameAtom of
        ?FULL ->
            % Write fake app file for the meta package
            AppFile = filename:join([
                TmpDir,
                "ebin",
                atom_to_list(?FULL) ++ ".app"
            ]),
            filelib:ensure_dir(AppFile),

            AppData =
                {application, ?FULL, [
                    {description, "Meta-package for Elixir"},
                    {vsn, binary_to_list(Tag)},
                    {modules, []},
                    {registered, []},
                    {applications, [kernel, stdlib]}
                ]},

            ok = file:write_file(
                AppFile,
                io_lib:format("~p.~n", [AppData])
            );
        _ ->
            extract_lib_from_pkg(Path, Name, TmpDir)
    end,

    Deps = [{N, {?RES, atom_to_binary(N), tag, Tag}} || N <- deps(NameAtom)],
    file:write_file(RebarConfig, io_lib:format("~p.", [{deps, Deps}])),

    ok.

make_vsn(_Param, _State) ->
    {error, "Replacing version of type elixir is not supported"}.

extract_lib_from_pkg(Filename, App, Dest) ->
    Prefix = lists:flatten(["lib/", binary_to_list(App), "/"]),
    PrefixLen = length(Prefix),

    {ok, _} = zip:foldl(
        fun(Name0, _GetInfo, GetBin, Acc) ->
            Name = ensure_string(Name0),
            case lists:prefix(Prefix, Name) andalso lists:last(Name) =/= $/ of
                true ->
                    rebar_api:debug("Found file ~s in zip", [Name]),

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

find_matching({tag, Tag}, _Builds) ->
    {tag, list_to_binary([Tag])};
find_matching(Requirement, Builds) ->
    {ok, Req0} = verl:parse_requirement(list_to_binary("~> " ++ Requirement)),
    Req1 = verl:compile_requirement(Req0),
    rebar_api:debug("Trying to find release from requirement ~s", [Requirement]),
    % TODO: Handle fully defined version? Cache release info?
    Builds0 = exerl_dep_builds:builds(Builds),

    Builds1 = [
        Build
     || Build <- Builds0,
        verl:is_match(exerl_dep_build:version(Build), Req1)
    ],

    case Builds1 of
        [] ->
            error(no_matching_builds);
        _ ->
            ok
    end,

    [BestMatch | _] = lists:sort(
        fun(Lhs, Rhs) ->
            verl:gt(exerl_dep_build:version(Lhs), exerl_dep_build:version(Rhs)) =:= true
        end,
        Builds1
    ),

    {tag, exerl_dep_build:tag(BestMatch)}.

-spec ensure_pkg(rebar_state:t(), exerl_dep_build:t()) -> file:filename_all().
ensure_pkg(State, Build) ->
    CacheDir = cache_dir(State),

    Tag = exerl_dep_build:tag(Build),

    Dest = list_to_binary(["elixir-", Tag, ".ez"]),
    DestPath = filename:join(CacheDir, Dest),

    case filelib:is_regular(DestPath) of
        true ->
            rebar_api:debug("File ~s exists", [DestPath]),
            ok;
        false ->
            filelib:ensure_dir(DestPath),
            DataUrl = exerl_dep_build:url(Build),

            rebar_api:debug("File ~s does not exist, downloading ~s", [DestPath, DataUrl]),
            exerl_dep_web:download_to_file(DataUrl, DestPath),

            Hash0 = binary:decode_hex(
                binary:part(
                    exerl_dep_build:sha256sum(Build), {0, 64}
                )
            ),

            % Verify checksum:
            {ok, Data} = file:read_file(DestPath),
            Hash1 = crypto:hash(sha256, Data),

            case Hash1 of
                Hash0 ->
                    % Checksum verified, all good
                    ok;
                _ ->
                    file:delete(DestPath),
                    file:delete(list_to_binary([DestPath, ".sha256sum"])),
                    error(checksum_failed)
            end
    end,
    DestPath.

-spec cache_dir(rebar_state:t()) -> file:filename_all().
cache_dir(State) ->
    Dir = rebar_dir:global_cache_dir(rebar_state:opts(State)),
    filename:join([
        Dir,
        "exerl",
        list_to_binary(["otp", erlang:system_info(otp_release)])
    ]).

deps(elixir) -> [];
deps(eex) -> [elixir];
deps(logger) -> [elixir];
deps(mix) -> [elixir, eex, logger];
deps(ex_unit) -> [mix];
deps(iex) -> [iex];
deps(elixir_full) -> [elixir, eex, iex, logger, mix, ex_unit].

ensure_string(V) when is_binary(V) ->
    binary_to_list(V);
ensure_string(V) when is_list(V) ->
    V.
