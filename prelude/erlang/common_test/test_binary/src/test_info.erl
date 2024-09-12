-module(test_info).

-export([load_from_file/1]).
-include_lib("common/include/buck_ct_records.hrl").

-type test_info() :: #test_info{}.
-export_type([test_info/0]).

-spec load_from_file(string()) -> #test_info{}.
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
        <<"erl_cmd">> := ErlCmd,
        <<"extra_flags">> := ExtraFlags,
        <<"artifact_annotation_mfa">> := ArtifactAnnotationMFA,
        <<"common_app_env">> := CommonAppEnv
    } = json:decode(Content),
    Providers1 = buck_ct_parser:parse_str(Providers),
    CtOpts1 = make_ct_opts(
        buck_ct_parser:parse_str(CtOpts),
        [buck_ct_parser:parse_str(CTH) || CTH <- ExtraCtHooks]
    ),
    #test_info{
        dependencies = [unicode:characters_to_list(filename:absname(Dep)) || Dep <- Dependencies],
        test_suite = filename:join(filename:absname(TestDir), [SuiteName, ".beam"]),
        config_files = lists:map(fun(ConfigFile) -> filename:absname(ConfigFile) end, ConfigFiles),
        providers = Providers1,
        artifact_annotation_mfa = parse_mfa(ArtifactAnnotationMFA),
        ct_opts = CtOpts1,
        erl_cmd = ErlCmd,
        extra_flags = ExtraFlags,
        common_app_env = CommonAppEnv
    }.

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
