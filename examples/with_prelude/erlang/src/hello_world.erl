% Copyright (c) Meta Platforms, Inc. and affiliates.
%
% This source code is dual-licensed under either the MIT license found in the
% LICENSE-MIT file in the root directory of this source tree or the Apache
% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
% of this source tree. You may select, at your option, one of the
% above-listed licenses.

% @format
-module(hello_world).
-compile(warn_missing_spec).

-export([main/1]).

-spec main([string()]) -> ok | no_return().
main([Name]) ->
    io:format("Hello, ~s!~n", [Name]);
main(_) ->
    usage().

-spec usage() -> no_return().
usage() ->
    io:format("Usage: ~s NAME~n", [escript:script_name()]),
    erlang:halt(1).
