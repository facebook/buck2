%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.

%% @format
-module(dependency_merger).

-export([main/1]).

-type filename() :: binary().

-spec main([string()]) -> ok | no_return().
main([OutFile, DepsFile, ExistingFile]) ->
    do(OutFile, DepsFile, ExistingFile);
main([OutFile, DepsFile]) ->
    do(OutFile, DepsFile);
main(_) ->
    usage(),
    erlang:halt(1).

-spec usage() -> ok.
usage() ->
    io:format("escript ~ts.erl out.json deps.json already_unionized.dep", [?MODULE]).

-spec do(file:filename(), file:filename(), file:filename()) -> ok.
do(OutFile0, DepsFile0, ExistingFile0) ->
    OutFile = dependency_utils:chars_to_binary(OutFile0),
    DepsFile = dependency_utils:chars_to_binary(DepsFile0),
    ExistingFile = dependency_utils:chars_to_binary(ExistingFile0),
    case read_file_term(ExistingFile) of
        {ok, Existing} ->
            do_collect(OutFile, DepsFile, Existing);
        Err ->
            io:format(standard_error, "error, could no parse file correctly: ~tp~n", [Err]),
            erlang:halt(1)
    end.

-spec do(file:filename(), file:filename()) -> ok.
do(OutFile0, DepsFile0) ->
    OutFile = dependency_utils:chars_to_binary(OutFile0),
    DepsFile = dependency_utils:chars_to_binary(DepsFile0),
    do_collect(OutFile, DepsFile, #{}).

-spec do_collect(filename(), filename(), map()) -> ok.
do_collect(OutFile, DepsFile, Existing) ->
    case read_file(DepsFile) of
        {ok, Deps} ->
            AllCollected = maps:fold(fun collect_files/3, Existing, Deps),
            Encoded = erlang:term_to_iovec(AllCollected, [deterministic]),
            ok = prim_file:write_file(OutFile, Encoded);
        Err ->
            io:format(standard_error, "error, could no parse file correctly: ~tp~n", [Err]),
            erlang:halt(1)
    end.

collect_files(File, DepFile, Acc) ->
    case prim_file:read_file(DepFile) of
        {ok, Bin} ->
            Acc#{File => Bin};
        Err ->
            io:format(standard_error, "error, could not read file: ~tp~n", [Err]),
            erlang:halt(1)
    end.

-spec read_file(filename()) -> {ok, map()} | {error, term()}.
read_file(File) ->
    case prim_file:read_file(File) of
        {ok, Data} ->
            {ok, json:decode(Data)};
        Err ->
            Err
    end.

-spec read_file_term(filename()) -> {ok, map()} | {error, term()}.
read_file_term(File) ->
    case prim_file:read_file(File) of
        {ok, Data} ->
            {ok, erlang:binary_to_term(Data)};
        Err ->
            Err
    end.
