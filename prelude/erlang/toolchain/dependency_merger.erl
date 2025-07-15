%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.

%% @format
-module(dependency_merger).

-export([main/1]).

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
    io:format("escript ~s.erl out.json deps.json already_unionized.dep", [?MODULE]).

-spec do(file:filename(), file:filename(), file:filename()) -> ok.
do(OutFile, DepsFile, ExistingFile) ->
    case read_file_term(ExistingFile) of
        {ok, Existing} ->
            do_collect(OutFile, DepsFile, Existing);
        Err ->
            io:format(standard_error, "error, could no parse file correctly: ~p~n", [Err]),
            erlang:halt(1)
    end.

-spec do(file:filename(), file:filename()) -> ok.
do(OutFile, DepsFile) ->
    do_collect(OutFile, DepsFile, #{}).

-spec do_collect(file:filename(), file:filename(), map()) -> ok.
do_collect(OutFile, DepsFile, Existing) ->
    case read_file(DepsFile) of
        {ok, Deps} ->
            AllCollected = maps:fold(fun collect_files/3, Existing, Deps),
            Encoded = erlang:term_to_binary(AllCollected, [deterministic]),
            ok = file:write_file(OutFile, Encoded, [raw]);
        Err ->
            io:format(standard_error, "error, could no parse file correctly: ~p~n", [Err]),
            erlang:halt(1)
    end.

collect_files(File, DepFile, Acc) ->
    case file:read_file(DepFile, [raw]) of
        {ok, Bin} ->
            Acc#{File => Bin};
        Err ->
            io:format(standard_error, "error, could not read file: ~p~n", [Err]),
            erlang:halt(1)
    end.

-spec read_file(file:filename()) -> {ok, map()} | {error, term()}.
read_file(File) ->
    case file:read_file(File, [raw]) of
        {ok, Data} ->
            {ok, json:decode(Data)};
        Err ->
            Err
    end.

-spec read_file_term(file:filename()) -> {ok, map()} | {error, term()}.
read_file_term(File) ->
    case file:read_file(File, [raw]) of
        {ok, Data} ->
            {ok, erlang:binary_to_term(Data)};
        Err ->
            Err
    end.
