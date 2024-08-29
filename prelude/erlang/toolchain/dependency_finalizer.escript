%%% % @format
%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.

-module(dependency_finalizer).
-author("loscher@meta.com").

-type dep_files_data() :: #{file:filename() => #{string() := file:filename()}}.

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
    case read_file(InFile) of
        {ok, DepFiles} ->
            Dependencies = build_dep_info(Source, DepFiles),
            OutData = unicode:characters_to_binary(json:encode(Dependencies)),
            case OutSpec of
                {file, File} ->
                    ok = file:write_file(File, OutData);
                stdout ->
                    io:format("~s~n", [OutData])
            end;
        Err ->
            io:format(standard_error, "error, could no parse file correctly: ~p~n", [Err]),
            erlang:halt(1)
    end.

-spec read_file(file:filename()) -> {ok, dep_files_data()} | {error, term()}.
read_file(File) ->
    case file:read_file(File) of
        {ok, Data} ->
            {ok, json:decode(Data)};
        Err ->
            Err
    end.

-spec build_dep_info(file:filename(), dep_files_data()) -> ok.
build_dep_info(Source, DepFiles) ->
    Key = list_to_binary(filename:basename(Source, ".erl") ++ ".beam"),
    collect_dependencies([Key], DepFiles, sets:new([{version, 2}]), []).

collect_dependencies([], _, _, Acc) ->
    Acc;
collect_dependencies([Key | Rest], DepFiles, Visited, Acc) ->
    case DepFiles of
        #{Key := #{<<"dep_file">> := DepFile}} ->
            {ok, Dependencies} = read_file(DepFile),
            {NextKeys, NextVisited, NextAcc} = lists:foldl(
                fun(#{<<"file">> := File} = Dep, {KeysAcc, VisitedAcc, DepAcc}) ->
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
