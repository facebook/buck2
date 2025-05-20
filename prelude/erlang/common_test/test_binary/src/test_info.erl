%% @format
-module(test_info).
-eqwalizer(ignore).

-export([load_from_file/1, write_to_file/2]).
-include_lib("common/include/buck_ct_records.hrl").

-type test_info() :: #test_info{}.
-export_type([test_info/0]).

-spec load_from_file(file:filename_all()) -> test_info().
load_from_file(TestInfoFile) ->
    {ok, Content} = file:read_file(TestInfoFile),
    #{
        <<"dependencies">> := Dependencies,
        <<"test_suite">> := SuiteName,
        <<"test_dir">> := TestDir,
        <<"config_files">> := ConfigFiles,
        <<"providers">> := Providers,
        <<"ct_opts">> := CtOpts,
        <<"extra_ct_hooks">> := ExtraCtHooks,
        <<"erl_cmd">> := [ErlExec | ErlFlags],
        <<"extra_flags">> := ExtraFlags,
        <<"artifact_annotation_mfa">> := ArtifactAnnotationMFA,
        <<"common_app_env">> := CommonAppEnv,
        <<"raw_target">> := RawTarget
    } = json:decode(Content),
    Providers1 = buck_ct_parser:parse_str(Providers),
    CtOpts1 = make_ct_opts(
        buck_ct_parser:parse_str(CtOpts),
        [buck_ct_parser:parse_str(CTH) || CTH <- ExtraCtHooks]
    ),

    #test_info{
        dependencies = [unicode:characters_to_list(make_path_absolute(Dep)) || Dep <- Dependencies],
        test_suite = filename:join((TestDir), [SuiteName, ".beam"]),
        config_files = [make_path_absolute(ConfigFile) || ConfigFile <- ConfigFiles],
        providers = Providers1,
        artifact_annotation_mfa = parse_mfa(ArtifactAnnotationMFA),
        ct_opts = CtOpts1,
        erl_cmd = [make_path_absolute(ErlExec) | ErlFlags],
        extra_flags = ExtraFlags,
        common_app_env = CommonAppEnv,
        raw_target = RawTarget
    }.

-spec write_to_file(file:filename_all(), test_info()) -> ok | {error, Reason :: term()}.
write_to_file(FileName, TestInfo) ->
    #test_info{
        dependencies = Dependencies,
        test_suite = SuiteBeamPath,
        config_files = ConfigFiles,
        providers = Providers,
        artifact_annotation_mfa = ArtifactAnnotationMFA,
        ct_opts = CtOpts,
        erl_cmd = [ErlCmd | ErlFlags],
        extra_flags = ExtraFlags,
        common_app_env = CommonAppEnv,
        raw_target = RawTarget
    } = TestInfo,
    ErlTermToStr = fun(Term) -> list_to_binary(lists:flatten(io_lib:format("~p", [Term]))) end,
    Json = #{
        <<"dependencies">> => [try_make_path_relative(Dep) || Dep <- Dependencies],
        <<"test_suite">> => filename:basename(SuiteBeamPath, ".beam"),
        <<"test_dir">> => filename:dirname(SuiteBeamPath),
        <<"config_files">> => [try_make_path_relative(ConfigFile) || ConfigFile <- ConfigFiles],
        <<"providers">> => ErlTermToStr(Providers),
        <<"ct_opts">> => ErlTermToStr(CtOpts),
        <<"extra_ct_hooks">> => [],
        <<"erl_cmd">> => [try_make_path_relative(ErlCmd) | ErlFlags],
        <<"extra_flags">> => ExtraFlags,
        <<"artifact_annotation_mfa">> => ErlTermToStr(ArtifactAnnotationMFA),
        <<"common_app_env">> => CommonAppEnv,
        <<"raw_target">> => RawTarget
    },
    file:write_file(FileName, json:encode(Json)).

-spec make_path_absolute(file:filename_all()) -> file:filename_all().
make_path_absolute(Path) ->
    case os:getenv("REPO_ROOT") of
        false -> filename:absname(Path);
        RepoRoot -> filename:join(RepoRoot, Path)
    end.

-spec try_make_path_relative(file:filename_all()) -> file:filename_all().
try_make_path_relative(Path) ->
    case filename:pathtype(Path) of
        relative ->
            Path;
        _ ->
            BaseDir =
                case os:getenv("REPO_ROOT") of
                    false ->
                        {ok, CWD} = file:get_cwd(),
                        CWD;
                    RepoRoot ->
                        RepoRoot
                end,
            BaseDirParts = filename:split(BaseDir),
            PathParts = filename:split(Path),
            case lists:split(length(BaseDirParts), PathParts) of
                {BaseDirParts, RelativeParts} -> filename:join(RelativeParts);
                _ -> Path
            end
    end.

-spec parse_mfa(binary()) -> artifact_annotations:annotation_function() | {error, term()}.
parse_mfa(MFA) ->
    case erl_scan:string(unicode:characters_to_list(MFA)) of
        {ok,
            [
                {'fun', _},
                {atom, _, Module},
                {':', _},
                {atom, _, Function},
                {'/', _},
                {integer, _, 1}
            ],
            _} ->
            fun Module:Function/1;
        {ok,
            [
                {atom, _, Module},
                {':', _},
                {atom, _, Function},
                {'/', _},
                {integer, _, 1}
            ],
            _} ->
            fun Module:Function/1;
        Reason ->
            {error, Reason}
    end.

-type ctopt() :: term().
-type cth() :: module() | {module(), term()}.

-spec make_ct_opts([ctopt()], [cth()]) -> [ctopt()].
make_ct_opts(CtOpts, []) -> CtOpts;
make_ct_opts(CtOpts, ExtraCtHooks) -> [{ct_hooks, ExtraCtHooks} | CtOpts].
