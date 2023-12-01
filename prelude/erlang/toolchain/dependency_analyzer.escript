%% #!/usr/bin/env escript
%% -*- erlang -*-
%%! +S 1:1 +sbtu +A1

%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%%% % @format
%%%-------------------------------------------------------------------
%%% @doc
%%%  Extract direct dependencies from a given erl or hrl file
%%%
%%%  usage:
%%%    dependency_analyzer.escript some_file.(h|e)rl [out.term]
%%%
%%%  The output of the tool is written either to stdout,
%%%  or a given output file. The format is as follows and intended to
%%%  be consumed by other file:consult/1:
%%% ```
%%%  [#{"type" := "include"
%%%             | "include_lib"
%%%             | "behaviour"
%%%             | "parse_transform"
%%%             | "manual_dependency",
%%%   "file"  := "header_or_source_file.(h|e)rl",
%%%  ["app"   => "application"][only for "include_lib"]
%%%   },
%%%   ...
%%%  ].
%%%  '''
%%% @end

-module(dependency_analyzer).
-author("loscher@fb.com").

%%% AST nodes to parse
%%%
%%% NOTE: This implementation parses the raw syntax tree structures from erl_syntax
%%%       instead of using the accessors, to use pattern-matching during the processing.

%% -include("header.hrl").
-define(MATCH_INCLUDE(Include),
    {tree, attribute, _, {attribute, {atom, _, include}, [{string, _, Include}]}}
).

%% -include_lib("app/include/header.hrl").
-define(MATCH_INCLUDE_LIB(Include),
    {tree, attribute, _, {attribute, {atom, _, include_lib}, [{string, _, Include}]}}
).

%% -behavior(Module).
-define(MATCH_BEHAVIOR(Module),
    {tree, attribute, _, {attribute, {tree, atom, _, behavior}, [{tree, atom, _, Module}]}}
).

%% -behaviour(Module).
-define(MATCH_BEHAVIOUR(Module),
    {tree, attribute, _, {attribute, {tree, atom, _, behaviour}, [{tree, atom, _, Module}]}}
).

%% -compile({parse_transform, Module}).
-define(MATCH_PARSETRANSFORM(Module),
    {tree, attribute, _,
        {attribute, {tree, atom, _, compile}, [
            {tree, tuple, _, [
                {tree, atom, _, parse_transform},
                {tree, atom, _, Module}
            ]}
        ]}}
).

%% -build_dependencies(Modules)
-define(MATCH_MANUAL_DEPENDENCIES(Modules),
    {tree, attribute, _, {attribute, {tree, atom, _, build_dependencies}, [{tree, list, _, {list, Modules, none}}]}}
).

%% entry point
-spec main([string()]) -> ok.
main([InFile]) ->
    do(InFile, stdout);
main([InFile, OutFile]) ->
    do(InFile, {file, OutFile});
main(_) ->
    usage(),
    erlang:halt(1).

-spec usage() -> ok.
usage() ->
    io:format("dependency_analyzer.escript some_file.(h|e)rl [out.term]").

-spec do(file:filename(), {file, file:filename()} | stdout) -> ok.
do(InFile, Outspec) ->
    {ok, Forms} = epp_dodger:parse_file(InFile),
    Dependencies = lists:sort(process_forms(Forms, [])),
    OutData = unicode:characters_to_binary(io_lib:format("~p.", [Dependencies])),
    case Outspec of
        {file, File} ->
            file:write_file(File, OutData);
        stdout ->
            io:format("~s~n", [OutData])
    end.

-spec process_forms(erl_syntax:forms(), [file:filename()]) -> [#{string() => string()}].
process_forms([], Acc) ->
    Acc;
process_forms([?MATCH_INCLUDE(Include) | Rest], Acc) ->
    Dependency = #{"file" => filename:basename(Include), "type" => "include"},
    process_forms(Rest, [Dependency | Acc]);
process_forms([?MATCH_INCLUDE_LIB(IncludeLib) | Rest], Acc) ->
    Dependency =
        case filename:split(IncludeLib) of
            [App, "include", Include] ->
                #{"app" => App, "file" => Include, "type" => "include_lib"};
            _ ->
                error(malformed_header_include_lib)
        end,
    process_forms(Rest, [Dependency | Acc]);
process_forms([?MATCH_BEHAVIOR(Module) | Rest], Acc) ->
    Dependency = #{"file" => module_to_erl(Module), "type" => "behaviour"},
    process_forms(Rest, [Dependency | Acc]);
process_forms([?MATCH_BEHAVIOUR(Module) | Rest], Acc) ->
    Dependency = #{"file" => module_to_erl(Module), "type" => "behaviour"},
    process_forms(Rest, [Dependency | Acc]);
process_forms([?MATCH_PARSETRANSFORM(Module) | Rest], Acc) ->
    Dependency = #{"file" => module_to_erl(Module), "type" => "parse_transform"},
    process_forms(Rest, [Dependency | Acc]);
process_forms([?MATCH_MANUAL_DEPENDENCIES(Modules) | Rest], Acc) ->
    Dependencies = [
        #{"file" => module_to_erl(Module), "type" => "manual_dependency"}
     || {tree, atom, _, Module} <- Modules
    ],
    process_forms(Rest, Dependencies ++ Acc);
process_forms([_ | Rest], Acc) ->
    process_forms(Rest, Acc).

-spec module_to_erl(module()) -> file:filename().
module_to_erl(Module) ->
    unicode:characters_to_list([atom_to_list(Module), ".erl"]).
