%% % @format
-module(common_util).

-export([unicode_characters_to_list/1]).
-compile(warn_missing_spec_all).

-spec unicode_characters_to_list(unicode:chardata()) -> string().
unicode_characters_to_list(CharData) ->
    case unicode:characters_to_list(CharData) of
        R when not is_tuple(R) -> R
    end.
