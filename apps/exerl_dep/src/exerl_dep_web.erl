-module(exerl_dep_web).

-export([
    % tls_opts/0,
    download_to_file/2,
    github_api/1
]).

-define(USER_AGENT, "exerl/0").

download_to_file(Url, Dest) ->
    DestS = binary_to_list(list_to_binary([Dest])),
    {ok, saved_to_file} = httpc:request(
        get,
        {
            Url,
            [{"User-Agent", ?USER_AGENT}]
        },
        [{ssl, tls_opts()}],
        [{stream, DestS}]
    ),
    ok.

-spec github_api(iodata()) -> thoas:json_term().
github_api(Path) ->
    Path1 =
        if
            is_binary(Path) -> Path;
            is_list(Path) -> list_to_binary(Path)
        end,

    Uri =
        case
            uri_string:recompose(#{
                scheme => "https",
                host => "api.github.com",
                path => Path1
            })
        of
            {error, ErrAtom, ErrTerm} ->
                error({ErrAtom, ErrTerm});
            Else ->
                Else
        end,

    Auth =
        case os:getenv("GITHUB_TOKEN") of
            false ->
                [];
            Token ->
                [{"Auth", "Bearer: " ++ Token}]
        end,

    {ok, Result} = httpc:request(
        get,
        {
            Uri,
            [
                {"Accept", "application/vnd.github+json"},
                {"X-GitHub-Api-Version", "2022-11-28"},
                {"User-Agent", ?USER_AGENT}
            ] ++ Auth
        },
        [{ssl, tls_opts()}],
        [{body_format, binary}]
    ),

    case Result of
        {{_, 200, _}, _Headers, Body} when is_binary(Body) ->
            {ok, Map} = thoas:decode(Body),
            Map;
        {{_, HttpCode, _}, _Headers, _Body} ->
            error({http_code, HttpCode})
    end.

%% @doc Return options for `ssl' functions that use the system certificate store
-spec tls_opts() -> [ssl:tls_option()].
tls_opts() ->
    try
        case erlang:function_exported(httpc, ssl_verify_host_options, 1) of
            true ->
                httpc:ssl_verify_host_options(true);
            false ->
                case erlang:function_exported(public_key, cacerts_get, 0) of
                    true ->
                        CaCerts = public_key:cacerts_get(),
                        [
                            {verify, verify_peer},
                            {cacerts, CaCerts},
                            {customize_hostname_check, [
                                {match_fun, public_key:pkix_verify_hostname_match_fun(https)}
                            ]}
                        ];
                    false ->
                        []
                end
        end
    catch
        _:{badmatch, {error, enoent}} ->
            []
    end.
