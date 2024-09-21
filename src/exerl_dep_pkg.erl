-module(exerl_dep_pkg).

-export([
    get_release/1,
    assets/1,
    version/1,
    tag/1,

    find_newest_version/0,
    find_newest_version/1,
    get_releases/0
]).

-record(release, {
    tag :: binary(),
    version :: verl:version_t(),
    assets :: #{binary() => binary()}
}).

get_release(Tag) when is_binary(Tag) ->
    Path = list_to_binary(["/repos/elixir-lang/elixir/releases/tags/", Tag]),
    GHRelease = exerl_dep_web:github_api(Path),
    {true, Release} = to_rec(GHRelease),
    Release.

to_rec(#{<<"prerelease">> := Pre}) when Pre =:= true ->
    false;
to_rec(#{<<"tag_name">> := Tag, <<"assets">> := Assets}) ->
    case verl:parse(binary:part(Tag, {1, byte_size(Tag) - 1})) of
        {ok, Version} ->
            {true, #release{
                tag = Tag,
                version = Version,
                assets = maps:from_list([
                    {Name, DownloadUrl}
                 || #{<<"name">> := Name, <<"browser_download_url">> := DownloadUrl} <- Assets
                ])
            }};
        _ ->
            false
    end.

assets(#release{assets = A}) -> A.
tag(#release{tag = T}) -> T.
version(#release{version = V}) -> V.

find_newest_version() ->
    find_newest_version([]).

find_newest_version(VersionPrefix) ->
    Releases = get_releases(),
    hd(
        lists:sort(
            fun(#{version := L}, #{version := R}) ->
                L > R
            end,
            [R || R = #{version := V} <- Releases, lists:prefix(VersionPrefix, V)]
        )
    ).

get_releases() ->
    Decoded = exerl_dep_web:github_api("/repos/elixir-lang/elixir/releases"),
    lists:filtermap(
        fun to_rec/1,
        Decoded
    ).
