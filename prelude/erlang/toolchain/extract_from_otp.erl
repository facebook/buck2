%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(extract_from_otp).
-author("loscher@meta.com").
-moduledoc """
Copy ERTS for releases to the given location

usage:
  extract_from_otp.erl wildcard target
""".

-export([main/1]).

-spec main([string()]) -> ok.
main([Wildcard, Target]) ->
    ok = extract(Wildcard, Target);
main(_) ->
    usage().

-spec extract(string(), string()) -> ok.
extract(Wildcard, Target) ->
    FullWildcard = filename:join(code:root_dir(), Wildcard),
    case filelib:wildcard(FullWildcard) of
        [OneResult] ->
            ok = copy_dir(OneResult, Target);
        Paths ->
            io:format("expected exactly one result but found: ~p~n", [Paths]),
            erlang:halt(1)
    end.

-spec usage() -> ok.
usage() ->
    io:format("needs exactly one argument: extract_from_otp.escript wildcard target~n").

copy_dir(From, To) ->
    Cmd = lists:flatten(
        io_lib:format("cp -r ~s ~s", [From, To])
    ),
    io:format("cmd: ~s~n~s~n~n", [Cmd, os:cmd(Cmd)]),
    case filelib:is_dir(To) of
        true -> ok;
        false -> erlang:halt(1)
    end.
