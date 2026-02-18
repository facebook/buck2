%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(app_src_builder).
-moduledoc """
Build an .app file from a given list of modules and a template
.app.src file.

usage:
  app_src_builder.escript app_info.json

app_info.json format:

  The file must contain only a single JSON map with the following spec:

 #{
     <<"name">>                   := <application_name>,
     <<"sources">>                := [<path to .erl source file>],
     <<"applications">>           := [<entry to applications field>],
     <<"included_applications">>  := I[<entry to included_applications field>],
     <<"template">>               => <path to an .app.src file>,
     <<"version">>                => <version string>,
     <<"env">>                    => [application env variable],
     <<"metadata">>               => map of metadata
 }
""".

-type application_resource() :: {application, atom(), proplists:proplist()}.
-type mod() :: {atom(), [term()]} | undefined.
-type filename() :: binary().

-export([main/1]).

-spec main([string()]) -> ok.
main([AppInfoFile, Output]) ->
    try
        do(dependency_utils:chars_to_binary(AppInfoFile), dependency_utils:chars_to_binary(Output))
    catch
        Type:{abort, Reason} ->
            io:format(standard_error, "~ts:~ts~n", [Type, Reason]),
            erlang:halt(1)
    end;
main(_) ->
    usage().

-spec usage() -> ok.
usage() ->
    io:format("app_src_builder.escript app_info.json~n").

-spec do(filename(), filename()) -> ok.
do(AppInfoFile, Output) ->
    #{
        name := Name,
        sources := Srcs,
        template := Template,
        vsn := Version,
        applications := Applications,
        included_applications := IncludedApplications,
        mod := Mod,
        env := Env,
        metadata := Metadata
    } = do_parse_app_info_file(AppInfoFile),
    VerifiedTerms = check_and_normalize_template(
        Name,
        Version,
        Template,
        Applications,
        IncludedApplications,
        Mod,
        Env,
        Metadata
    ),
    render_app_file(Name, VerifiedTerms, Output, Srcs).

-spec do_parse_app_info_file(filename()) ->
    #{
        name := string(),
        vsn := string(),
        sources := [filename()]
    }.
do_parse_app_info_file(AppInfoFile) ->
    case prim_file:read_file(AppInfoFile) of
        {ok, Content} ->
            case json:decode(Content) of
                #{
                    <<"name">> := Name,
                    <<"sources">> := Sources,
                    <<"applications">> := Applications,
                    <<"included_applications">> := IncludedApplications
                } = Terms ->
                    Template = get_template(maps:get(<<"template">>, Terms, undefined)),
                    Mod = get_mod(Name, maps:get(<<"mod">>, Terms, undefined)),
                    Env = get_env(Name, maps:get(<<"env">>, Terms, undefined)),
                    Metadata = get_metadata(Name, maps:get(<<"metadata">>, Terms, undefined)),
                    #{
                        name => Name,
                        sources => Sources,
                        vsn => maps:get(<<"version">>, Terms, undefined),
                        template => Template,
                        applications =>
                            normalize_application([binary_to_atom(App) || App <- Applications]),
                        included_applications =>
                            [binary_to_atom(App) || App <- IncludedApplications],
                        mod => Mod,
                        env => Env,
                        metadata => Metadata
                    };
                Terms ->
                    file_corrupt_error(AppInfoFile, Terms)
            end;
        Error ->
            open_file_error(AppInfoFile, Error)
    end.

-spec get_template(filename() | undefined) -> application_resource().
get_template(undefined) ->
    {application, '_', []};
get_template(TemplateFile) ->
    case file:consult(TemplateFile) of
        {ok, [Template]} -> Template;
        {ok, Terms} -> file_corrupt_error(TemplateFile, Terms);
        Error -> open_file_error(TemplateFile, Error)
    end.

-spec get_mod(binary(), [binary() | [binary()]] | undefined) -> mod().
get_mod(_, undefined) ->
    undefined;
get_mod(AppName, [ModuleName, StringArgs]) ->
    parse_term(
        AppName,
        ["{", ModuleName, ", ", StringArgs, "}"],
        "mod field"
    ).

-spec parse_term(binary(), iolist(), string()) -> term().
parse_term(AppName, RawString, ErrorDescription) ->
    String = unicode:characters_to_list([RawString | "."]),
    try
        {ok, Tokens, _EndLine} = erl_scan:string(String),
        {ok, Term} = erl_parse:parse_term(Tokens),
        Term
    catch
        _:_ -> parse_error(AppName, String, ErrorDescription)
    end.

-spec get_env(binary(), map() | undefined) -> [tuple()] | undefined.
get_env(_Name, undefined) ->
    undefined;
get_env(Name, Env) ->
    [
        {binary_to_atom(K), parse_term(Name, V, io_lib:format("env value for ~ts", [K]))}
     || K := V <- maps:iterator(Env, ordered)
    ].

-spec get_metadata(binary(), map() | undefined) -> map().
get_metadata(_Name, undefined) -> #{};
get_metadata(Name, Metadata) -> #{binary_to_atom(K) => normalize_metadata_value(Name, K, V) || K := V <- Metadata}.

-spec normalize_metadata_value(binary(), binary(), binary() | [binary()]) -> atom() | [atom()].
normalize_metadata_value(AppName, Key, Value) when is_binary(Value) ->
    parse_term(AppName, Value, io_lib:format("metadata value for ~ts", [Key]));
normalize_metadata_value(AppName, Key, Values) when is_list(Values) ->
    Value = ["[", lists:join(",", Values), "]"],
    parse_term(AppName, Value, io_lib:format("metadata value for ~ts", [Key])).

-spec check_and_normalize_template(
    binary(),
    binary() | undefined,
    term(),
    [atom()],
    [atom()],
    mod(),
    [tuple()],
    map()
) ->
    application_resource().
check_and_normalize_template(
    AppName,
    TargetVersion,
    Terms,
    Applications,
    IncludedApplications,
    Mod,
    Env,
    Metadata
) ->
    App = binary_to_atom(AppName),
    Props =
        case Terms of
            {application, App, P} when erlang:is_list(P) ->
                P;
            {application, '_', P} when erlang:is_list(P) ->
                P;
            _ ->
                Msg = io_lib:format(
                    "expect the top-level format of the template to be {application, '~ts'/'_', [ ... ]}.~nBut got instead: ~tp",
                    [
                        AppName,
                        Terms
                    ]
                ),
                erlang:error(
                    {abort, Msg}
                )
        end,
    VerifiedProps = verify_app_props(
        AppName, TargetVersion, Applications, IncludedApplications, Props
    ),
    Props0 = add_optional_fields(VerifiedProps, [{mod, Mod}, {env, Env}]),
    Props1 = add_metadata(Props0, Metadata),
    {application, App, Props1}.

-spec add_optional_fields(proplists:proplist(), mod() | [tuple()]) -> proplists:proplist().
add_optional_fields(Props, []) ->
    Props;
add_optional_fields(Props, [{_, undefined} | Fields]) ->
    add_optional_fields(Props, Fields);
add_optional_fields(Props, [{K, V0} | Fields]) ->
    V1 = proplists:get_value(K, Props, undefined),
    case V1 of
        undefined ->
            add_optional_fields([{K, V0} | Props], Fields);
        % overwrite the value of empty list in .app.src, for example: {env, []}
        [] ->
            add_optional_fields([{K, V0} | Props], Fields);
        _ ->
            case V0 =:= V1 of
                true -> add_optional_fields(Props, Fields);
                false -> erlang:error(app_props_not_compatible, [{K, V0}, {K, V1}])
            end
    end;
add_optional_fields(Props, [Field | Fields]) ->
    add_optional_fields([Field | Props], Fields).

-spec verify_app_props(binary(), binary(), [atom()], [atom()], proplists:proplist()) -> ok.
verify_app_props(AppName, Version, Applications, IncludedApplications, Props0) ->
    Props1 = verify_applications(AppName, Props0),
    %% ensure defaults
    ensure_fields(AppName, Version, Applications, IncludedApplications, Props1).

-spec verify_applications(binary(), proplists:proplist()) -> ok.
verify_applications(AppName, AppDetail) ->
    case proplists:get_value(applications, AppDetail) of
        AppList when is_list(AppList) ->
            FinalApps = normalize_application(AppList),
            lists:keystore(applications, 1, AppDetail, {applications, FinalApps});
        undefined ->
            AppDetail;
        BadApplicationsValue ->
            applications_type_error(AppName, BadApplicationsValue)
    end.

-spec normalize_application(list(atom())) -> list(atom()).
normalize_application(Applications) ->
    NormalizedApplications0 =
        case lists:member(stdlib, Applications) of
            false ->
                [stdlib | Applications];
            true ->
                Applications
        end,
    NormalizedApplications =
        case lists:member(kernel, Applications) of
            false ->
                [kernel | NormalizedApplications0];
            true ->
                NormalizedApplications0
        end,
    NormalizedApplications.

-spec ensure_fields(binary(), binary(), [atom()], [atom()], proplists:proplist()) ->
    proplists:proplist().
ensure_fields(AppName, Version, Applications, IncludedApplications, Props) ->
    %% default means to add the value if not existing
    %% match meand to overwrite if not existing and check otherwise for
    Defaults = [
        {{registered, []}, default},
        {{vsn, binary_to_list(Version)}, match},
        {{description, "missing description"}, default},
        {{applications, Applications}, match},
        {{included_applications, IncludedApplications}, match}
    ],
    lists:foldl(
        fun
            ({{Key, _} = Default, default}, Acc) ->
                case lists:keyfind(Key, 1, Acc) of
                    false -> [Default | Acc];
                    _ -> Acc
                end;
            ({{Key, Value} = Default, match}, Acc) ->
                case lists:keyfind(Key, 1, Acc) of
                    false ->
                        [Default | Acc];
                    {Key, Value} ->
                        Acc;
                    %% When 'git' is specified as the version in the .app.src file, it means that
                    %% the version will be calculated dynamically based on the VCS version.
                    %% We consider the version from the Buck target to be authoritative.
                    {vsn, Vsn} when Vsn =:= git orelse Vsn =:= "git" ->
                        [Default | lists:keydelete(vsn, 1, Acc)];
                    Wrong ->
                        value_match_error(AppName, Wrong, Default)
                end
        end,
        Props,
        Defaults
    ).

-spec render_app_file(string(), application_resource(), filename(), [filename()]) ->
    ok.
render_app_file(AppName, Terms, Output, Srcs) ->
    App = binary_to_atom(AppName),
    Modules = generate_modules(Srcs),
    {application, App, Props0} = Terms,
    %% remove modules key
    Props1 = lists:keydelete(modules, 1, Props0),
    %% construct new terms
    Spec =
        {application, App, [{modules, Modules} | Props1]},
    ToWrite = io_lib:format("~kp.\n", [Spec]),
    ok = prim_file:write_file(Output, ToWrite).

-spec generate_modules([filename()]) -> [atom()].
generate_modules(Sources) ->
    Modules = lists:foldl(
        fun(Source, Acc) ->
            case filename:extension(Source) of
                <<".hrl">> ->
                    Acc;
                Ext when Ext =:= <<".erl">> orelse Ext =:= <<".xrl">> orelse Ext =:= <<".yrl">> ->
                    ModuleName = filename:basename(Source, Ext),
                    Module = erlang:binary_to_atom(ModuleName),
                    [Module | Acc];
                _ ->
                    unknown_extension_error(Source)
            end
        end,
        [],
        Sources
    ),
    lists:usort(Modules).

-spec unknown_extension_error(File :: filename()) -> no_return().
unknown_extension_error(File) ->
    Msg = io_lib:format("unsupported extension for source ~ts", [File]),
    erlang:error(
        {abort, Msg}
    ).

-spec open_file_error(File :: filename(), Error :: term()) -> no_return().
open_file_error(File, Error) ->
    Msg = io_lib:format("cannot open file ~ts: ~tp", [File, Error]),
    erlang:error(
        {abort, Msg}
    ).

-spec file_corrupt_error(File :: filename(), Contents :: term()) -> no_return().
file_corrupt_error(File, Contents) ->
    Msg = io_lib:format("corrupt information in ~ts: ~tp", [File, Contents]),
    erlang:error(
        {abort, Msg}
    ).

-spec value_match_error(binary(), {atom(), term()}, {atom(), term()}) -> no_return().
value_match_error(AppName, Wrong = {_, Value1}, Default = {_, Value2}) when
    is_list(Value1) andalso is_list(Value2)
->
    case io_lib:printable_list(Value1) andalso io_lib:printable_list(Value2) of
        true -> value_match_error_scalar(AppName, Wrong, Default);
        false -> value_match_error_diff(AppName, Wrong, Default)
    end;
value_match_error(AppName, Wrong, Default) ->
    value_match_error_scalar(AppName, Wrong, Default).

value_match_error_diff(AppName, {FieldName, Value1}, {FieldName, Value2}) ->
    Diff = diff_list(Value1, Value2),
    Msg = io_lib:format(
        ("error when building ~ts.app for application ~ts: the field ~ts in "
        "the app.src template does not match with the target definition"),
        [
            AppName, AppName, FieldName
        ]
    ),
    erlang:error(
        {abort, [Msg, "\n", Diff]}
    ).

value_match_error_scalar(AppName, {FieldName, Value1}, {FieldName, Value2}) ->
    Msg = io_lib:format(
        ("error when building ~ts.app for application ~ts: the field ~ts in the "
        "app.src template (~tp) does not match with the target definition (~tp)"),
        [
            AppName, AppName, FieldName, Value1, Value2
        ]
    ),
    erlang:error(
        {abort, Msg}
    ).

-spec applications_type_error(string(), term()) -> no_return().
applications_type_error(AppName, Applications) ->
    Msg = io_lib:format(
        "error when building ~ts.app for application ~ts: require a list for applications value but got ~tw instead",
        [
            AppName, AppName, Applications
        ]
    ),
    erlang:error(
        {abort, Msg}
    ).

-spec parse_error(string(), string(), string()) -> no_return().
parse_error(AppName, String, Description) ->
    Msg = io_lib:format(
        "error when building ~ts.app for application ~ts: could not parse value for ~ts: `~tp`",
        [
            AppName, AppName, Description, String
        ]
    ),
    erlang:error(
        {abort, Msg}
    ).

diff_list(AppSrcValue, TargetValue) ->
    LCS = lcs(AppSrcValue, TargetValue),
    DiffSpec = construct_diff_spec(LCS, AppSrcValue, TargetValue, []),
    construct_diff(DiffSpec).

construct_diff_spec([], [], [], Acc) ->
    lists:reverse(Acc);
construct_diff_spec([], [RemoveItem | AppSrcValue], TargetValue, Acc) ->
    construct_diff_spec([], AppSrcValue, TargetValue, [{remove, RemoveItem} | Acc]);
construct_diff_spec([], [], [AddItem | TargetValue], Acc) ->
    construct_diff_spec([], [], TargetValue, [{add, AddItem} | Acc]);
construct_diff_spec(
    [CommonItem | LCS], [CommonItem | AppSrcValue], [CommonItem | TargetValue], Acc
) ->
    NewAcc =
        case Acc of
            [{common, N, CommonItems} | Rest] ->
                [{common, N + 1, [CommonItem | CommonItems]} | Rest];
            _ ->
                [{common, 1, [CommonItem]} | Acc]
        end,
    construct_diff_spec(LCS, AppSrcValue, TargetValue, NewAcc);
construct_diff_spec([CommonItem | _] = LCS, [RemoveItem | AppSrcValue], TargetValue, Acc) when
    CommonItem =/= RemoveItem
->
    construct_diff_spec(LCS, AppSrcValue, TargetValue, [{remove, RemoveItem} | Acc]);
construct_diff_spec([CommonItem | _] = LCS, AppSrcValue, [AddItem | TargetValue], Acc) when
    CommonItem =/= AddItem
->
    construct_diff_spec(LCS, AppSrcValue, TargetValue, [{add, AddItem} | Acc]).

% 10 spaces
-define(LPAD, "          ").
% 28 spaces
-define(MPAD, "                            ").
% 45 spaces
-define(RPAD, "                                             ").

construct_diff(Spec) ->
    Header = [
        io_lib:format("           .app.src                           buck2 target~n", []),
        io_lib:format("           ========                           ============~n", [])
    ],
    construct_diff(Spec, Header).

construct_diff(L, Header) ->
    Diff =
        [
            case E of
                {common, N, Items} when N < 5 ->
                    [io_lib:format(" ~ts~ts~n", [?MPAD, format_item(Item)]) || Item <- Items];
                {common, N, Items} ->
                    [Last | _] = Items,
                    First = lists:last(Items),
                    [
                        io_lib:format(" ~ts~ts~n", [?MPAD, format_item(First)]),
                        io_lib:format(" ~ts~ts~n", [?MPAD, io_lib:format("... ~b more ...", [N - 2])]),
                        io_lib:format(" ~ts~ts~n", [?MPAD, format_item(Last)])
                    ];
                {remove, Item} ->
                    io_lib:format("<~ts~ts~n", [?LPAD, format_item(Item)]);
                {add, Item} ->
                    io_lib:format(">~ts~ts~n", [?RPAD, format_item(Item)])
            end
         || E <- L
        ],
    [Header | Diff].

format_item(Item) ->
    S = io_lib:format("~tw", [Item]),
    case string:length(S) > 30 of
        true ->
            io_lib:format("~.27ts...", [S]);
        false ->
            io_lib:format("~.30ts", [S])
    end.

%% longest common subsequence from http://rosettacode.org/wiki/Longest_common_subsequence#Erlang
lcs_length([] = S, T, Cache) ->
    {0, maps:put({S, T}, 0, Cache)};
lcs_length(S, [] = T, Cache) ->
    {0, maps:put({S, T}, 0, Cache)};
lcs_length([H | ST] = S, [H | TT] = T, Cache) ->
    {L, C} = lcs_length(ST, TT, Cache),
    {L + 1, maps:put({S, T}, L + 1, C)};
lcs_length([_SH | ST] = S, [_TH | TT] = T, Cache) ->
    case maps:is_key({S, T}, Cache) of
        true ->
            {maps:get({S, T}, Cache), Cache};
        false ->
            {L1, C1} = lcs_length(S, TT, Cache),
            {L2, C2} = lcs_length(ST, T, C1),
            L = max(L1, L2),
            {L, maps:put({S, T}, L, C2)}
    end.

lcs(S, T) ->
    {_, C} = lcs_length(S, T, #{}),
    lcs(S, T, C, []).

lcs([], _, _, Acc) ->
    lists:reverse(Acc);
lcs(_, [], _, Acc) ->
    lists:reverse(Acc);
lcs([H | ST], [H | TT], Cache, Acc) ->
    lcs(ST, TT, Cache, [H | Acc]);
lcs([_SH | ST] = S, [_TH | TT] = T, Cache, Acc) ->
    case maps:get({S, TT}, Cache) > maps:get({ST, T}, Cache) of
        true ->
            lcs(S, TT, Cache, Acc);
        false ->
            lcs(ST, T, Cache, Acc)
    end.

-spec add_metadata(proplists:proplist(), map()) -> proplists:proplist().
add_metadata(Props, Metadata) ->
    ok = verify_metadata(Props, Metadata),
    Props ++ maps:to_list(maps:iterator(Metadata, ordered)).

-spec verify_metadata(proplists:proplist(), map()) -> ok.
verify_metadata([], _) ->
    ok;
verify_metadata([{K, V0} | T], Metadata) ->
    case maps:get(K, Metadata, undefined) of
        undefined ->
            verify_metadata(T, Metadata);
        V1 ->
            case V0 =:= V1 of
                true ->
                    verify_metadata(T, Metadata);
                false ->
                    erlang:error(metadata_not_compatible, [{K, V0}, {K, V1}])
            end
    end.
