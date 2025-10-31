%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(shell_buck2_module_search).
-moduledoc """
Configurable hook for module discovery
""".
-compile(warn_missing_spec_all).

-export([find_module/1, find_module_source/1]).

-import(common_util, [unicode_characters_to_binary/1]).

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
            case shell_buck2_utils:get_app_env(search_module) of
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
    io:format("use ~ts as root", [Root]),
    {ok, Output} = shell_buck2_utils:run_command([
        "find",
        Root,
        "-type",
        "d",
        "(",
        "-path",
        io_lib:format("~ts/_build*", [Root]),
        "-o",
        "-path",
        io_lib:format("~ts/erl/_build*", [Root]),
        "-o",
        "-path",
        io_lib:format("~ts/buck-out", [Root]),
        ")",
        "-prune",
        "-o",
        "-name",
        io_lib:format("~ts.erl", [Module]),
        "-print"
    ]),
    case
        [
            RelPath
         || Path <- string:split(Output, ~"\n", all),
            RelPath <- [unicode_characters_to_binary(P) || P <- [string:prefix(Path, [Root, ~"/"])], P =/= nomatch],
            string:prefix(RelPath, ~"buck-out") == nomatch,
            binary:match(RelPath, ~"_build") == nomatch
        ]
    of
        [ModulePath] ->
            {source, ModulePath};
        [] ->
            {error, not_found};
        Candidates ->
            %% check if there are actually targets associated
            {ok, RawOutput} = shell_buck2_utils:buck2_query(
                ~|owner(\\"%s\\")|,
                [~"--json"],
                Candidates
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
