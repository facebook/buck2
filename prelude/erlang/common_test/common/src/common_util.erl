%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% % @format
-module(common_util).

-export([
    unicode_characters_to_list/1,
    unicode_characters_to_binary/1,

    filename_all_to_filename/1
]).
-compile(warn_missing_spec_all).

-spec unicode_characters_to_list(unicode:chardata()) -> string().
unicode_characters_to_list(CharData) ->
    case unicode:characters_to_list(CharData) of
        R when not is_tuple(R) -> R
    end.

-spec unicode_characters_to_binary(unicode:chardata()) -> binary().
unicode_characters_to_binary(Chars) ->
    case unicode:characters_to_binary(Chars) of
        Bin when is_binary(Bin) -> Bin
    end.

-spec filename_all_to_filename(file:filename_all()) -> file:filename().
filename_all_to_filename(Filename) when is_binary(Filename) ->
    unicode_characters_to_list(Filename);
filename_all_to_filename(Filename) ->
    Filename.
