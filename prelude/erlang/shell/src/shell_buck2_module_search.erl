%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(shell_buck2_module_search).

-export([find_module/1, find_module_source/1]).

-compile(warn_missing_spec_all).
-moduledoc """
Configurable hook for module discovery
""".

-eqwalizer(ignore).

-callback find_module_source(module()) ->
    {source, file:filename_all()}
    | {error, not_found | {ambiguous, [file:filename_all()]}}.

-spec find_module(atom()) ->
    available
    | {source, file:filename_all()}
    | {error, not_found | {ambiguous, [file:filename_all()]}}.
find_module(Module) ->
    WantedModuleName = atom_to_list(Module),
    case
        [
            found
         || {ModuleName, _, _} <- code:all_available(),
            string:equal(WantedModuleName, ModuleName)
        ]
    of
        [found] ->
            available;
        _ ->
            _ = application:load(buck2_shell_utils),
            % elp:ignore W0011 (application_get_env)
            case application:get_env(buck2_shell_utils, search_module) of
                {ok, Mod} ->
                    Mod:find_module_source(Module);
                _ ->
                    find_module_source(Module)
            end
    end.

-spec find_module_source(module()) ->
    {source, file:filename_all()}
    | {error, not_found | {ambiguous, [file:filename_all()]}}.
find_module_source(Module) ->
    Root = shell_buck2_utils:cell_root(),
    io:format("use ~s as root", [Root]),
    {ok, Output} = shell_buck2_utils:run_command(
        "find ~s -type d "
        "\\( -path \"~s/_build*\" -path \"~s/erl/_build*\" -o -path ~s/buck-out \\) -prune "
        "-o -name '~s.erl' -print",
        [Root, Root, Root, Root, Module]
    ),
    case
        [
            case unicode:characters_to_list(RelPath) of
                Value when not is_tuple(Value) -> Value
            end
         || RelPath <- [
                string:prefix(Path, [Root, "/"])
             || Path <- string:split(Output, "\n", all)
            ],
            RelPath =/= nomatch,
            string:prefix(RelPath, "buck-out") == nomatch,
            string:str(binary_to_list(RelPath), "_build") == 0
        ]
    of
        [ModulePath] ->
            {source, ModulePath};
        [] ->
            {error, not_found};
        Candidates ->
            %% check if there are actually targets associated
            {ok, RawOutput} = shell_buck2_utils:buck2_query(
                "owner(\\\"\%s\\\")", "--json", Candidates
            ),
            SourceTargetMapping = json:decode(RawOutput),
            case
                maps:fold(
                    fun
                        (_Source, [], Acc) -> Acc;
                        (Source, _, Acc) -> [Source | Acc]
                    end,
                    [],
                    SourceTargetMapping
                )
            of
                [] -> {error, not_found};
                [Source] -> {source, Source};
                More -> {error, {ambiguous, More}}
            end
    end.
