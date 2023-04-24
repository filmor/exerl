%% @moduledoc Rebar3 integration
-module(exerl_r3).

-export([
    init/1,
    ensure_pkg/2
]).

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    % os:putenv("MIX_ENV", "prod"),
    BaseDir = rebar_dir:base_dir(State),
    DepsDir = rebar_dir:deps_dir(State),
    os:putenv("MIX_DEPS_PATH", filename:absname(DepsDir)),
    os:putenv("MIX_BUILD_PATH", filename:absname(BaseDir)),

    % TODO: Either ensure that dependencies are used (resources) or gather from
    % executable.
    %
    % set_code_path(),
    %
    % TODO: Load on-demand instead
    % {ok, _} = application:ensure_all_started(mix),
    State1 = rebar_state:prepend_compilers(State, [exerl_r3_compile]),
    State2 = rebar_state:add_resource(State1, {elixir, exerl_r3_resource}),
    State3 = rebar_state:add_project_builder(State2, mix, exerl_r3_builder),
    % exerl_mix_converger:register(),
    {ok, State3}.

ensure_pkg(State, Version) ->
    CacheDir = cache_dir(State),

    Dest = list_to_binary(["elixir-", Version, ".ez"]),
    DestPath = filename:join(CacheDir, Dest),

    case filelib:is_regular(DestPath) of
        true ->
            ok;
        false ->
            OtpVersion = erlang:system_info(otp_release),
            DataName = list_to_binary(["elixir-otp-", OtpVersion, ".zip"]),
            ChecksumName = <<DataName/binary, ".sha256sum">>,

            Rel = exerl_pkg:get_release(list_to_binary(["v", Version])),
            Assets = exerl_pkg:assets(Rel),
            DataUrl = maps:get(DataName, Assets),
            ChecksumUrl = maps:get(ChecksumName, Assets),

            filelib:ensure_dir(DestPath),
            exerl_util:download_to_file(DataUrl, DestPath),
            exerl_util:download_to_file(ChecksumUrl, [DestPath, ".sha256sum"]),

            % Verify checksum:
            {ok, Data} = file:read_file(DestPath),
            Hash0 = crypto:hash(sha256, Data),

            {ok, SumData} = file:read_file(list_to_binary([DestPath, ".sha256sum"])),
            % First 64 bytes decoded
            Hash1 = binary:decode_hex(binary:part(SumData, {0, 64})),

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

cache_dir(State) ->
    Dir = rebar_dir:global_cache_dir(rebar_state:opts(State)),
    filename:join([
        Dir,
        "exerl",
        list_to_binary(["otp", erlang:system_info(otp_release)])
    ]).
