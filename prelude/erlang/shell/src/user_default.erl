%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% % @format
-module(user_default).
-wacov(ignore).

-compile(warn_missing_spec_all).

-eqwalizer(ignore).

-export([
    c/1, c/2, c/3,
    l/1
]).

-type c_ret() :: {ok, module()} | error.
-type c_options() :: [compile:option()] | compile:option().

-spec c(module()) -> c_ret().
c(Module) ->
    c(Module, []).

-spec c(module(), c_options()) -> c_ret().
c(Module, Options) ->
    c(Module, Options, fun(_) -> true end).

-spec c(module(), c_options(), fun((compile:option()) -> boolean())) -> c_ret().
c(Module, _Options, _Filter) ->
    case code:which(Module) of
        non_existing ->
            {error, non_existing};
        _ ->
            case shell_buck2_utils:rebuild_modules([Module]) of
                ok ->
                    ok = ct_daemon:push_module(Module),
                    code:purge(Module),
                    code:load_file(Module),
                    {ok, Module};
                error ->
                    error
            end
    end.

-spec l(module()) -> code:load_ret().
l(Module) ->
    case shell_buck2_module_search:find_module(Module) of
        available ->
            c:l(Module);
        {source, RelSource} ->
            AbsSource = filename:absname(RelSource),
            Paths = shell_buck2_utils:get_additional_paths(AbsSource),
            ok = code:add_paths(Paths),
            ok = ct_daemon:push_paths(Paths),
            c:l(Module);
        Error ->
            Error
    end.
