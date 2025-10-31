%% Copyright (c) Meta Platforms, Inc. and affiliates.
%%
%% This source code is licensed under both the MIT license found in the
%% LICENSE-MIT file in the root directory of this source tree and the Apache
%% License, Version 2.0 found in the LICENSE-APACHE file in the root directory
%% of this source tree.

%% @format
-module(artifact_annotations).
-compile(warn_missing_spec_all).
-moduledoc """
This file acts as a manual erlang sync for the thrift type struct
TestResultArtifactAnnotations defined in  https://fburl.com/code/r2t4vclb

 == How To Update This File ==
We mostly expect next iterations of thrift data structure to include
more testArtifactTypes. Those should be manually added to the
test_artifact_type() here.
""".

-import(common_util, [unicode_characters_to_binary/1]).

-type generic_blob() :: #{generic_blob := #{}}.
-type generic_text_log() :: #{generic_text_log := #{}}.
-type test_artifact_type() :: generic_blob() | generic_text_log().

-type test_result_artifact_annotations() :: #{
    type := test_artifact_type(), description := binary()
}.

-type annotation_function() :: fun((file:filename()) -> test_result_artifact_annotations()).

%% Public API
-export([serialize/1, create_artifact_annotation/2, default_annotation/1]).
-export_type([
    annotation_function/0,
    test_result_artifact_annotations/0
]).

-spec serialize(test_result_artifact_annotations()) -> iodata().
serialize(ArtifactAnnotation) -> json:encode(ArtifactAnnotation).

-spec create_artifact_annotation(FileName, AnnotationFunction) -> test_result_artifact_annotations() when
    FileName :: file:filename(),
    AnnotationFunction :: annotation_function().
create_artifact_annotation(FileName, AnnotationFunction) ->
    AnnotationFunction(FileName).

-spec default_annotation(FileName :: file:filename()) -> test_result_artifact_annotations().
default_annotation(FileName) ->
    Type =
        case lists:member(filename:extension(FileName), [".json", ".html", ".log", ".spec", ".txt"]) of
            true -> #{generic_text_log => #{}};
            _ -> #{generic_blob => #{}}
        end,
    #{
        type => Type,
        description => unicode_characters_to_binary(FileName)
    }.
