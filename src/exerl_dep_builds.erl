-module(exerl_dep_builds).

-export([
    new/1,
    builds/1,
    update/1,
    ensure/1
]).

-export_type([t/0]).

-define(BUILDS_URI, "https://builds.hex.pm/builds/elixir/builds.txt").
-define(USER_AGENT, "exerl/0").

-record(state, {
    cache_path :: file:name_all(),
    etag_path :: file:name_all(),
    etag :: undefined | binary(),
    builds :: undefined | [exerl_dep_build:t()]
}).
-opaque t() :: #state{}.

-spec new(rebar_state:t()) -> t().
new(RebarState) ->
    {CacheFname, EtagFname} = get_paths(RebarState),
    #state{cache_path = CacheFname, etag_path = EtagFname}.

-spec builds(t()) -> [exerl_dep_build:t()].
builds(State) ->
    case (ensure(State))#state.builds of
        undefined ->
            error(failed_to_load_builds);
        L ->
            L
    end.

-spec update(t()) -> t().
update(#state{etag = Etag} = S) ->
    case maybe_download_builds(Etag, S) of
        {ok, _} ->
            read(S);
        {error, Err} ->
            error(Err)
    end.

-spec ensure(t()) -> t().
ensure(State) ->
    State1 = read(State),

    case State1 of
        #state{builds = Builds} when is_list(Builds) ->
            State1;
        _ ->
            update(State)
    end.

-spec read(t()) -> t().
read(State = #state{cache_path = CacheFname, etag_path = EtagFname}) ->
    case filelib:is_regular(CacheFname) of
        true ->
            Etag =
                case file:read_file(EtagFname) of
                    {ok, Etag1} ->
                        Etag1;
                    _ ->
                        undefined
                end,

            case file:read_file(CacheFname) of
                {ok, Cache} ->
                    case parse(Cache) of
                        {ok, Builds} ->
                            State#state{
                                builds = Builds, etag = Etag
                            };
                        _Err ->
                            State
                    end;
                _Err ->
                    State
            end;
        false ->
            State
    end.

maybe_download_builds(Etag, S) ->
    Headers = [{"User-Agent", ?USER_AGENT}],
    Headers1 =
        case Etag of
            undefined ->
                Headers;
            _ ->
                [{"If-None-Match", Etag} | Headers]
        end,

    rebar_api:debug("[exerl] Headers: ~p", [Headers1]),

    case
        httpc:request(
            get,
            {?BUILDS_URI, Headers1},
            [{ssl, exerl_dep_web:tls_opts()}],
            []
        )
    of
        {ok, {{_, 200, _}, HeadersIn, Body}} when is_binary(Body), is_list(HeadersIn) ->
            rebar_api:debug("[exerl] Got new builds file", []),
            NewEtag = proplists:get_value("etag", HeadersIn),

            #state{cache_path = CacheFname, etag_path = EtagFname} = S,
            ok = filelib:ensure_dir(EtagFname),
            ok = filelib:ensure_dir(CacheFname),

            ok = file:write_file(EtagFname, NewEtag),
            ok = file:write_file(CacheFname, Body),

            % Updated, write file and etag
            {ok, updated};
        {ok, {{_, 304, _}, _, _}} ->
            % Not modified, keep files as they are
            {ok, not_updated};
        {ok, {{_, Code, Message}, _, Body}} ->
            rebar_api:warn(
                "[exerl] Failed to download builds file: ~p ~s\n~s", [Code, Message, Body]
            );
        Err ->
            rebar_api:warn(
                "[exerl] Failed to download builds file: ~p", [Err]
            )
    end.

parse(Data) ->
    case lists:filtermap(fun exerl_dep_build:parse/1, string:split(Data, "\n", all)) of
        [] ->
            {error, empty_cache};
        L ->
            {ok, L}
    end.

get_paths(State) ->
    Dir = rebar_dir:global_cache_dir(rebar_state:opts(State)),
    {
        filename:join([Dir, "exerl", "builds.txt"]),
        filename:join([Dir, "exerl", "builds.txt.etag"])
    }.
