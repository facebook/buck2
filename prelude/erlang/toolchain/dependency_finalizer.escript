%%% % @format

-module(dependency_finalizer).
-author("loscher@meta.com").

-spec main([string()]) -> ok | no_return().
main([Source, InFile]) ->
    do(Source, InFile, stdout);
main([Source, InFile, OutFile]) ->
    do(Source, InFile, {file, OutFile});
main(_) ->
    usage(),
    erlang:halt(1).

-spec usage() -> ok.
usage() ->
    io:format("~s.escript <source> dependency_spec.term [out.json]", [?MODULE]).

-spec do(file:filename(), file:filename(), {file, file:filename()} | stdout) -> ok.
do(Source, InFile, OutSpec) ->
    case file:consult(InFile) of
        {ok, [DepFiles]} ->
            Dependencies = build_dep_info(Source, DepFiles),
            OutData = unicode:characters_to_binary(to_json_list(Dependencies)),
            case OutSpec of
                {file, File} ->
                    file:write_file(File, OutData);
                stdout ->
                    io:format("~s~n", [OutData])
            end;
        Err ->
            io:format(stderr, "error, could no parse file correctly: ~p~n", [Err]),
            erlang:halt(1)
    end.

-spec build_dep_info(file:filename(), #{file:filename() => #{string() := file:filename()}}) -> ok.
build_dep_info(Source, DepFiles) ->
    Key = filename:basename(Source, ".erl") ++ ".beam",
    collect_dependencies([Key], DepFiles, sets:new([{version, 2}]), []).

collect_dependencies([], _, _, Acc) ->
    Acc;
collect_dependencies([Key | Rest], DepFiles, Visited, Acc) ->
    case DepFiles of
        #{Key := #{"dep_file" := DepFile}} ->
            {ok, [Dependencies]} = file:consult(DepFile),
            {NextKeys, NextVisited, NextAcc} = lists:foldl(
                fun(#{"file" := File} = Dep, {KeysAcc, VisitedAcc, DepAcc}) ->
                    NextKey = key(File),
                    case sets:is_element(NextKey, VisitedAcc) of
                        true -> {KeysAcc, VisitedAcc, DepAcc};
                        false -> {[NextKey | KeysAcc], sets:add_element(Key, VisitedAcc), [Dep | DepAcc]}
                    end
                end,
                {Rest, Visited, Acc},
                Dependencies
            ),
            collect_dependencies(
                NextKeys,
                DepFiles,
                NextVisited,
                NextAcc
            );
        _ ->
            %% We cannot find key in DepFiles, which means it's from OTP or missing
            %% We don't add anything to the dependencies and let the compiler fail for a proper error message.
            collect_dependencies(Rest, DepFiles, Visited, Acc)
    end.

-spec key(string()) -> string().
key(FileName) ->
    case filename:extension(FileName) of
        ".erl" -> filename:basename(FileName, ".erl") ++ ".beam";
        _ -> FileName
    end.

%%%
%%% JSON encoding: base-line escripts we use in our toolchain need to be dependency less
%%%

-spec to_json_list([#{string() => string()}]) -> string().
to_json_list(Dependencies) ->
    [
        "[",
        string:join([json_encode_dependency(Dependency) || Dependency <- Dependencies], ","),
        "]"
    ].

-spec json_encode_dependency(#{string() => string()}) -> string().
json_encode_dependency(Dep) ->
    Elements = maps:fold(
        fun(Key, Value, Acc) ->
            [[json_string_escape(Key), ":", json_string_escape(Value)] | Acc]
        end,
        [],
        Dep
    ),
    ["{", string:join(Elements, ","), "}"].

-spec json_string_escape(string()) -> string().
json_string_escape(Str) ->
    [
        "\"",
        [json_escape_char(C) || C <- Str],
        "\""
    ].

-spec json_escape_char(non_neg_integer()) -> non_neg_integer() | string().
json_escape_char($\") ->
    [$\\, $\"];
json_escape_char($\\) ->
    [$\\, $\\];
json_escape_char($\/) ->
    [$\\, $\/];
json_escape_char($\b) ->
    [$\\, $\b];
json_escape_char($\f) ->
    [$\\, $\f];
json_escape_char($\n) ->
    [$\\, $\n];
json_escape_char($\r) ->
    [$\\, $\r];
json_escape_char($\t) ->
    [$\\, $\t];
json_escape_char(C) when C >= 16#20 andalso C =< 16#10FFFF ->
    %% unescaped, 16#5C (\) and 16#22 (") are handled above
    C;
json_escape_char(C) when C < 16#10000 ->
    io_lib:format("\\u~s", [string:pad(integer_to_list(C, 16), 4, leading, " ")]);
json_escape_char(_) ->
    %% TODO: support extended unicode characters
    error(utf8_extended_character_not_supported).
