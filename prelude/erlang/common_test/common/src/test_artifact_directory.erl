%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%%%-------------------------------------------------------------------
%%% @doc
%%% Artefact directory file management.
%%% Used by TPX to upload diagnostic reports.
%%% @end
%%% % @format

-module(test_artifact_directory).
-compile(warn_missing_spec).

-include_lib("kernel/include/logger.hrl").
-include_lib("common/include/buck_ct_records.hrl").

-import(common_util, [unicode_characters_to_list/1]).

%% Public API
-export([prepare/2, link_to_artifact_dir/3]).

-export_type([dir_path/0]).

-type dir_path() :: file:filename() | undefined.

% Gets the artifactory directory path.
% This one might be undefined if tpx is ran in offline mode.
-spec artifact_dir() -> dir_path().
artifact_dir() ->
    ArtifactDir = os:getenv("TEST_RESULT_ARTIFACTS_DIR"),
    case ArtifactDir of
        false ->
            undefined;
        Dir ->
            filelib:ensure_path(Dir),
            Dir
    end.

-spec with_artifact_dir(fun((file:filename()) -> X)) -> X | ok.
with_artifact_dir(Func) ->
    case artifact_dir() of
        undefined -> ok;
        Dir -> Func(Dir)
    end.

-spec artifact_annotation_dir() -> dir_path().
artifact_annotation_dir() ->
    ArtifactAnnotationDir = os:getenv("TEST_RESULT_ARTIFACT_ANNOTATIONS_DIR"),
    case ArtifactAnnotationDir of
        false ->
            undefined;
        Dir ->
            filelib:ensure_path(Dir),
            Dir
    end.

-spec with_artifact_annotation_dir(fun((file:filename()) -> X)) -> X | ok.
with_artifact_annotation_dir(Func) ->
    case artifact_annotation_dir() of
        undefined -> ok;
        Dir -> Func(Dir)
    end.

% Collect, create and link the logs and other relevant files in
% the artefacts directory.
-spec prepare(file:filename(), #test_env{}) -> ok.
prepare(ExecutionDir, TestInfo) ->
    with_artifact_dir(
        fun(_ArtifactDir) ->
            link_tar_ball(ExecutionDir),
            case find_log_private(ExecutionDir) of
                {error, log_private_not_found} ->
                    ok;
                LogPrivate ->
                    [
                        link_to_artifact_dir(File, LogPrivate, TestInfo)
                     || File <- filelib:wildcard(join_paths(LogPrivate, "**/*.log")),
                        filelib:is_regular(File)
                    ]
            end,
            ok
        end
    ).

-spec link_to_artifact_dir(file:filename_all(), file:filename_all(), #test_env{}) -> ok.
link_to_artifact_dir(File, Root, TestEnv) ->
    with_artifact_dir(
        fun(ArtifactDir) ->
            RelativePath =
                case string:prefix(File, Root) of
                    nomatch ->
                        ?LOG_ERROR("~s should be a prefix of ~s", [Root, File]),
                        error(unexpected_path);
                    Suffix ->
                        string:strip(unicode_characters_to_list(Suffix), left, $/)
                end,
            FullFileName =
                unicode_characters_to_list(string:replace(RelativePath, "/", ".", all)),
            case filelib:is_file(File) of
                true ->
                    file:make_symlink(File, join_paths(ArtifactDir, FullFileName)),
                    Annotation = artifact_annotations:create_artifact_annotation(FullFileName, TestEnv),
                    dump_annotation(Annotation, FullFileName);
                _ ->
                    ok
            end
        end
    ).

-spec link_tar_ball(file:filename()) -> ok.
link_tar_ball(LogDir) ->
    with_artifact_dir(
        fun(ArtifactDir) ->
            {_Pid, MonitorRef} = spawn_monitor(fun() ->
                {ok, Items} = file:list_dir(LogDir),
                Files = [
                    {Item, filename:join(LogDir, Item)}
                 || Item <- Items, not string:equal(filename:join(LogDir, Item), ArtifactDir)
                ],
                erl_tar:create(
                    filename:join(ArtifactDir, "execution_dir.tar.gz"),
                    Files,
                    [compressed]
                )
            end),
            receive
                {'DOWN', MonitorRef, _Type, _Object, _Info} -> ok
            after 15000 -> ok
            end
        end
    ).

-spec dump_annotation(artifact_annotations:test_result_artifact_annotations(), file:filename()) -> ok.
dump_annotation(Annotation, FileName) ->
    with_artifact_annotation_dir(
        fun(ArtifactAnnotationDir) ->
            AnnotationName = FileName ++ ".annotation",
            {ok, AnnotationFile} = file:open(
                filename:join(ArtifactAnnotationDir, AnnotationName), [write]
            ),
            file:write(AnnotationFile, artifact_annotations:serialize(Annotation)),
            ok
        end
    ).

-spec find_log_private(file:filename()) -> {error, log_private_not_found} | file:filename().
find_log_private(LogDir) ->
    Candidates = [
        Folder
     || Folder <- filelib:wildcard(join_paths(LogDir, "**/log_private")), filelib:is_dir(Folder)
    ],
    case Candidates of
        [] -> {error, log_private_not_found};
        [LogPrivate | _Tail] -> LogPrivate
    end.

-spec join_paths(file:filename_all(), file:filename_all()) -> string().
join_paths(Path, FileName) ->
    case filename:join(Path, FileName) of
        NewPath when is_list(NewPath) -> NewPath
    end.
