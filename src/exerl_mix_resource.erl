-module(exerl_mix_resource).

-behaviour(rebar_resource_v2).

-export([init/2, lock/2, download/4, needs_update/2, make_vsn/2]).

-spec init(atom(), rebar_state:t()) -> {ok, rebar_resource_v2:resource()}.
init(Type, State) ->
    Module =
        case Type of
            git -> rebar_git_resource;
            git_subdir -> rebar_git_subdir_resource
        end,
    {ok, Resource} = Module:init(Type, State),
    ResourceState = rebar_resource_v2:find_resource_state(Type, [Resource]),
    {ok, rebar_resource_v2:new(Type, ?MODULE, {Module, ResourceState})}.

-spec lock(rebar_app_info:t(), {module(), term()}) -> rebar_resource_v2:source().
lock(AppInfo, {Module, ResourceState}) ->
    Module:lock(AppInfo, ResourceState).

-spec download(file:filename_all(), rebar_app_info:t(), rebar_state:t(), {module(), term()}) ->
    ok | {error, term()}.
download(TmpDir, AppInfo, State, {Module, ResourceState}) ->
    case Module:download(TmpDir, AppInfo, State, ResourceState) of
        ok ->
            Dir =
                case rebar_app_info:source(AppInfo) of
                    {git_subdir, _, _, SubDir} -> filename:join(TmpDir, SubDir);
                    _ -> TmpDir
                end,
            exerl_mix_metadata:generate(Dir);
        {error, _} = Error ->
            Error
    end.

-spec needs_update(rebar_app_info:t(), {module(), term()}) -> boolean().
needs_update(AppInfo, {Module, ResourceState}) ->
    Module:needs_update(AppInfo, ResourceState).

-spec make_vsn(rebar_app_info:t(), {module(), term()}) ->
    {plain, string()} | {error, string()}.
make_vsn(AppInfo, {Module, ResourceState}) ->
    Module:make_vsn(AppInfo, ResourceState).
