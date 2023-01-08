-module(exerl_tls).

-export([
    opts/0
]).

%% @doc Return options for `ssl' functions that use the system certificate store
-spec opts() -> [ssl:tls_option()].
opts() ->
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
    end.
