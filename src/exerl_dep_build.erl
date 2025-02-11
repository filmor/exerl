-module(exerl_dep_build).

-export([
    parse/1,
    version/1,
    tag/1,
    url/1,
    sha256sum/1,

    get_releases/1
    % find_newest_version/0,
    % find_newest_version/1
]).

-export_type([t/0]).

-record(build, {
    name :: binary(),
    version :: verl:version_t() | undefined,
    version_raw :: binary() | undefined,
    otp_version :: non_neg_integer() | undefined,
    commit :: binary(),
    sha256sum :: binary() | undefined,
    timestamp :: binary()
}).
-opaque t() :: #build{}.

get_releases(Builds) ->
    OtpVersion = list_to_integer(erlang:system_info(otp_release)),

    Builds = exerl_dep_builds:builds(Builds),

    % Filter out all different otp versions and prereleases
    [
        Build
     || Build <- Builds,
        Build#build.otp_version =:= OtpVersion,
        maps:get(pre, Build#build.version) =:= []
    ].

version(#build{version = V}) -> V.
tag(#build{version_raw = N}) -> N.
sha256sum(#build{sha256sum = S}) -> S.

url(B) ->
    list_to_binary(["https://builds.hex.pm/builds/elixir/", B#build.name, ".zip"]).

parse(Row) ->
    case string:split(Row, " ", all) of
        [RefOtp, Commit, Timestamp, Sha256Sum | _] ->
            make_record(RefOtp, Commit, Timestamp, Sha256Sum);
        [RefOtp, Commit, Timestamp] ->
            make_record(RefOtp, Commit, Timestamp, undefined);
        [<<>>] ->
            % Skipping empty row
            false;
        Else ->
            rebar_api:error("[exerl] Failed to parse build row: ~p", [Else]),
            false
    end.

make_record(RefOtp, Commit, Timestamp, Sha256Sum) ->
    expand_record(#build{
        name = RefOtp,
        commit = Commit,
        timestamp = Timestamp,
        sha256sum = Sha256Sum
    }).

expand_record(Build = #build{name = RefOtp}) ->
    case string:split(RefOtp, "-otp-") of
        [Ref, Otp] ->
            case verl:parse(string:trim(Ref, leading, "v")) of
                {ok, Version} ->
                    {true, Build#build{
                        name = Ref,
                        version = Version,
                        version_raw = Ref,
                        otp_version = binary_to_integer(Otp)
                    }};
                Err ->
                    rebar_api:debug("[exerl] Failed to parse version ~s: ~p", [Ref, Err]),
                    false
            end;
        _ ->
            false
    end.

% find_newest_version() ->
%     find_newest_version([]).
%
% find_newest_version(VersionPrefix) ->
%     Releases = get_releases(),
%     hd(
%         lists:sort(
%             fun(#{version := L}, #{version := R}) ->
%                 L > R
%             end,
%             [R || R = #{version := V} <- Releases, lists:prefix(VersionPrefix, V)]
%         )
%     ).
