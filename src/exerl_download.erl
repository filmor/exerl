-module(exerl_download).

-export([
    download_and_extract/0,
    download_and_extract/1,
    find_newest_version/0,
    find_newest_version/1,
    get_releases/0
]).

download_and_extract() ->
    download_and_extract([]).

download_and_extract(VersionPrefix) when is_list(VersionPrefix) ->
    Rel = find_newest_version(VersionPrefix),
    download_and_extract(Rel);
download_and_extract(Release) ->
    #{
        version := Version,
        data_url := DataUrl,
        checksum_url := ChecksumUrl
    } = Release,

    OutPath = exerl_path:elixir_path(Version),

    case filelib:is_dir(OutPath) of
        true ->
            ok;
        false ->
            Temp = exerl_temp:mkdtemp(),
            ReleaseZip = filename:join([Temp, "release.zip"]),

            httpc:request(
                get,
                {
                    DataUrl,
                    [{"User-Agent", "exerl/0"}]
                },
                [{ssl, exerl_tls:opts()}],
                [{stream, ReleaseZip}]
            ),

            case ChecksumUrl of
                not_found ->
                    ok;
                _ ->
                    {ok, Result} = httpc:request(
                        get,
                        {
                            DataUrl,
                            [{"User-Agent", "exerl/0"}]
                        },
                        [{ssl, exerl_tls:opts()}],
                        [{body_format, binary}]
                    ),
                    {{_, 200, _}, _Headers, _Body} = Result
                % TODO: Use crypto to check the hash
            end,

            file:del_dir_r(OutPath),
            filelib:ensure_path(filename:join([OutPath, "dir"])),

            zip:extract(ReleaseZip, [{cwd, OutPath}])
    end.

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
    {ok, Result} = httpc:request(
        get,
        {
            <<"https://api.github.com/repos/elixir-lang/elixir/releases">>,
            [
                {"Accept", "application/vnd.github+json"},
                {"X-GitHub-Api-Version", "2022-11-28"},
                {"User-Agent", "exerl/0"}
            ]
        },
        [{ssl, exerl_tls:opts()}],
        [{body_format, binary}]
    ),

    {{_, 200, _}, _Headers, Body} = Result,

    Decoded = jsone:decode(Body),

    OtpVersion = erlang:system_info(otp_release),
    DataUrl = list_to_binary(
        io_lib:format("elixir-otp-~s.zip", [OtpVersion])
    ),
    ChecksumUrl = <<DataUrl/binary, ".sha256sum">>,

    [
        #{
            version => parse_version(maps:get(<<"tag_name">>, T)),
            data_url =>
                case get_url_by_filename(T, DataUrl) of
                    not_found ->
                        get_url_by_filename(T, <<"Precompiled.zip">>);
                    Else ->
                        Else
                end,
            checksum_url => get_url_by_filename(T, ChecksumUrl)
        }
     || T = #{<<"prerelease">> := Pre} <- Decoded, not Pre
    ].

get_url_by_filename(#{<<"assets">> := Assets}, Filename) ->
    try
        lists:foreach(
            fun
                (#{<<"name">> := Name} = A) when Name =:= Filename ->
                    throw({found, A});
                (#{<<"name">> := _}) ->
                    ok
            end,
            Assets
        ),
        not_found
    catch
        {found, #{<<"browser_download_url">> := Url}} ->
            Url
    end.

parse_version(<<"v", Rest/binary>>) ->
    [
        element(1, string:to_integer(I))
     || I <- string:split(Rest, ".", all)
    ].
