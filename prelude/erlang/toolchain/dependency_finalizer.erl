%%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%%
%%% This source code is licensed under both the MIT license found in the
%%% LICENSE-MIT file in the root directory of this source tree and the Apache
%%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%%% of this source tree.

%% @format
-module(dependency_finalizer).
-author("loscher@meta.com").

-export([main/1]).

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
            FlatDepFiles = lists:foldl(fun maps:merge/2, #{}, maps:values(DepFiles)),
            Dependencies = build_dep_info(Source, FlatDepFiles),
            OutData = json:encode(Dependencies),
            case OutSpec of
                {file, File} ->
                    ok = write_file(File, OutData);
                stdout ->
                    io:format("~s~n", [OutData])
            end;
        Err ->
            io:format(standard_error, "error, could no parse file correctly: ~p~n", [Err]),
            erlang:halt(1)
    end.

-spec read_file(file:filename()) -> {ok, dep_files_data()} | {error, term()}.
read_file(File) ->
    case file:read_file(File, [raw]) of
        {ok, Data} ->
            {ok, json:decode(Data)};
        Err ->
            Err
    end.

-spec build_dep_info(file:filename(), dep_files_data()) -> list(map()).
build_dep_info(Source, DepFiles) ->
    Key = list_to_binary(filename:basename(Source, ".erl") ++ ".beam"),
    collect_dependencies([Key], DepFiles, sets:new([{version, 2}]), []).

collect_dependencies([], _, _, Acc) ->
    Acc;
collect_dependencies([Key | Rest], DepFiles, Visited, Acc) ->
    case DepFiles of
        #{Key := DepFile} ->
            {ok, Dependencies} = read_file(DepFile),
            {NextKeys, NextVisited, NextAcc} =
                collect_dependencies_for_key(Dependencies, Key, Rest, Visited, Acc),
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

collect_dependencies_for_key([], _CurrentKey, KeysAcc, VisitedAcc, DepAcc) ->
    {KeysAcc, VisitedAcc, DepAcc};
collect_dependencies_for_key([#{<<"file">> := File} = Dep | Deps], CurrentKey, KeysAcc, VisitedAcc, DepAcc) ->
    NextKey = key(File),
    case sets:is_element(NextKey, VisitedAcc) of
        true ->
            collect_dependencies_for_key(Deps, CurrentKey, KeysAcc, VisitedAcc, DepAcc);
        false ->
            collect_dependencies_for_key(Deps, CurrentKey, [NextKey | KeysAcc], sets:add_element(CurrentKey, VisitedAcc), [Dep | DepAcc])
    end.

-spec key(string()) -> string().
key(FileName) ->
    case filename:extension(FileName) of
        ".erl" -> filename:basename(FileName, ".erl") ++ ".beam";
        _ -> FileName
    end.

-spec write_file(file:filename(), iolist()) -> string().
write_file(File, Data) ->
    case
        % We write in raw mode because this is a standalone escript, so we don't
        % need advanced file server features, and we gain performance by avoiding
        % calling through the file server
        file:open(File, [write, binary, raw])
    of
        {ok, Handle} ->
            try
                % We use file:pwrite instead of file:write_file to work around
                % the latter needlessly flattening iolists (as returned by
                % json:encode/1, etc.) to a binary
                file:pwrite(Handle, 0, Data)
            after
                file:close(Handle)
            end,
            ok;
        {error, _} = Error ->
            Error
    end.
