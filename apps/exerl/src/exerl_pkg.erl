-module(exerl_pkg).

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
    version :: verl:version(),
    assets :: #{binary() => binary()}
}).

download_release(#release{assets = Assets}, Dest) ->
    OtpVersion = erlang:system_info(otp_release),
    DataName = list_to_binary(io_lib:format("elixir-otp-~s.zip", [OtpVersion])),
    ChecksumName = <<DataName/binary, ".sha256sum">>,
    DataUrl = maps:get(DataName, Assets),
    ChecksumUrl = maps:get(ChecksumName, Assets),

    filelib:ensure_dir(Dest),

    exerl_util:download_to_file(DataUrl, Dest),

    ok;
download_release(Tag, Dest) when is_binary(Tag) ->
    download_release(get_release(Tag), Dest).

get_release(Tag) when is_binary(Tag) ->
    Path = list_to_binary(["/repos/elixir-lang/elixir/releases/tags/", Tag]),
    GHRelease = exerl_util:github_api(Path),
    to_rec(GHRelease).

to_rec(#{<<"tag_name">> := Tag, <<"assets">> := Assets}) ->
    {ok, Version} = verl:parse(binary:part(Tag, {1, byte_size(Tag) - 1})),
    #release{
        tag = Tag,
        version = Version,
        assets = maps:from_list([
            {Name, DownloadUrl}
         || #{<<"name">> := Name, <<"browser_download_url">> := DownloadUrl} <- Assets
        ])
    }.

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
    Decoded = exerl_util:github_api("/repos/elixir-lang/elixir/releases"),
    [to_rec(T) || T = #{<<"prerelease">> := Pre} <- Decoded, not Pre].
