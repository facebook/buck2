# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    "@prelude//linking:link_info.bzl",
    "SharedLibLinkable",  # @unused Used as a type
)

EagerBitcodeLinkData = record(
    name = str,
    input_object_file = Artifact,  # This may be a native object file or a bitcode file, we can't tell at this point
    output_index_shard_file = Artifact,
    plan = Artifact,
    output_final_native_object_file = Artifact,
    merged_bc = field([Artifact, None]),
)

LazyBitcodeLinkData = record(
    name = str,
    input_object_file = Artifact,  # This may be a native object file or a bitcode file, we can't tell at this point
    output_index_shard_file = Artifact,
    plan = Artifact,
    output_final_native_object_file = Artifact,
    merged_bc = field([Artifact, None]),
    archive_start = bool,
    archive_end = bool,
)

ArchiveLinkData = record(
    name = str,
    manifest = Artifact,
    input_object_files_dir = Artifact,
    output_final_native_object_files_dir = Artifact,
    output_index_shard_files_dir = Artifact,
    plan = Artifact,
    link_whole = bool,
    merged_bc_dir = field([Artifact, None]),
)

DynamicLibraryLinkData = record(
    linkable = SharedLibLinkable,
)

LinkDataType = enum(
    "eager_bitcode",
    "lazy_bitcode",
    "archive",
    "dynamic_library",
)

DThinLTOLinkData = record(
    data_type = LinkDataType,
    link_data = field([LazyBitcodeLinkData, EagerBitcodeLinkData, ArchiveLinkData, DynamicLibraryLinkData]),
)
