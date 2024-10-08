-module(exerl_dep_web).

-export([
    tls_opts/0,
    download_to_file/2
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
