#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""
A wrapper script around the linker invocation that completes distributed thin-lto thin-link.

This script:

(1) Parses the meta file, which specifies various file paths for each input object file.
(2) Writes "plan" json files for each input object file or archive describing to Buck starlark logic how to create opt + codegen actions for each
(3) Writes a "link plan" and a "final index". The link plan is used to identify which input object files are already native object files, and the final index constitutes a filelist used in the final native link. This filelist is a transformed version of the "index.full" file the linker produces.

Starlark code holds a representation of each input object file or archive in memory in an array. When code here needs to communicate characteristics about a particular element of this array, it encodes this using the index into this array. These indices are referred to as "starlark array index"
"""

# pyre-unsafe

import argparse
import json
import os
import os.path
import subprocess
import sys
import tempfile
from dataclasses import dataclass
from enum import Enum
from typing import Optional


class BitcodeMergeState(str, Enum):
    STANDALONE = "STANDALONE"
    ABSORBED = "ABSORBED"
    ROOT = "ROOT"
    NOT_LOADED = "NOT_LOADED"


def is_file_llvm_bitcode_wrapper_file(filepath: str) -> bool:
    LLVM_BITCODE_WRAPPER_FILE_MAGIC = 0xDEC0170B
    with open(filepath, "rb") as f:
        file_magic = int.from_bytes(f.read(4), "big")
        return file_magic == LLVM_BITCODE_WRAPPER_FILE_MAGIC


def read_merged_bitcode_file(merged_bitcode_path) -> BitcodeMergeState:
    if os.path.getsize(merged_bitcode_path) == 0:
        return BitcodeMergeState.NOT_LOADED

    if is_file_llvm_bitcode_wrapper_file(merged_bitcode_path):
        return BitcodeMergeState.ROOT

    with open(merged_bitcode_path) as merged_bitcode_file:
        for line in merged_bitcode_file:
            if "standalone" in line:
                return BitcodeMergeState.STANDALONE
            if "absorbed" in line:
                return BitcodeMergeState.ABSORBED

    raise Exception(f"unexpected merged bitcode file contents: {merged_bitcode_path}")


class MetafileRecordType(str, Enum):
    LAZY = "LAZY"
    EAGER = "EAGER"


@dataclass
class LazyObjectFile:
    """A lazy object file is an object file passed to the link in an archive."""

    record_type: MetafileRecordType
    input_object_file_path: str
    output_index_shards_directory_path: str
    output_premerged_bitcode_directory_path: Optional[str]
    output_plan_file_path: str
    starlark_array_index: int

    def __init__(
        self,
        record_type: str,
        input_object_file_path: str,
        output_index_shards_directory_path: str,
        output_plan_file_path: str,
        starlark_array_index: int,
        output_premerged_bitcode_directory_path: Optional[str] = None,
    ):
        self.record_type = MetafileRecordType(record_type)
        self.input_object_file_path = input_object_file_path
        self.output_index_shards_directory_path = output_index_shards_directory_path
        self.output_premerged_bitcode_directory_path = (
            output_premerged_bitcode_directory_path
        )
        self.output_plan_file_path = output_plan_file_path
        self.starlark_array_index = int(starlark_array_index)


@dataclass
class EagerObjectFile:
    """An eager object file is an object file passed to the link directly, not as part of an archive."""

    record_type: MetafileRecordType
    input_object_file_path: str
    output_index_shard_file_path: str
    output_premerged_bitcode_file_path: Optional[str]
    output_plan_file_path: str
    starlark_array_index: int

    def __init__(
        self,
        record_type: str,
        input_object_file_path: str,
        output_index_shard_file_path: str,
        output_plan_file_path: str,
        starlark_array_index: int,
        output_premerged_bitcode_file_path: Optional[str] = None,
    ):
        self.record_type = MetafileRecordType(record_type)
        self.input_object_file_path = input_object_file_path
        self.output_index_shard_file_path = output_index_shard_file_path
        self.output_premerged_bitcode_file_path = output_premerged_bitcode_file_path
        self.output_plan_file_path = output_plan_file_path
        self.starlark_array_index = int(starlark_array_index)


@dataclass
class Archive:
    """An archive is a collection of lazy object files that are passed to the link as a single unit."""

    objects: list[LazyObjectFile]
    output_index_shards_directory_path: str
    output_premerged_bitcode_directory_path: Optional[str]
    output_plan_file_path: str


BITCODE_SUFFIX = ".thinlto.bc"
IMPORTS_SUFFIX = ".imports"
OPT_OBJECTS_SUFFIX = ".opt.o"  # please note the files are not exist yet, this is to generate the index file use in final link
MERGED_BITCODE_SUFFIX = ".merged.bc"


def _parse_meta_file_records(
    meta_file_path: str,
) -> tuple[dict[str, EagerObjectFile | LazyObjectFile], dict[int, Archive]]:
    object_file_records_map: dict[str, EagerObjectFile | LazyObjectFile] = {}
    archive_records_map: dict[int, Archive] = {}

    with open(meta_file_path) as meta:
        object_file_records_json = json.load(meta)
        for record in object_file_records_json:
            if record["record_type"] == MetafileRecordType.LAZY.value:
                lazy_object_file_record = LazyObjectFile(**record)
                object_file_records_map[
                    lazy_object_file_record.input_object_file_path
                ] = lazy_object_file_record
                if lazy_object_file_record.starlark_array_index in archive_records_map:
                    archive_records_map[
                        lazy_object_file_record.starlark_array_index
                    ].objects.append(lazy_object_file_record)
                else:
                    archive_records_map[
                        lazy_object_file_record.starlark_array_index
                    ] = Archive(
                        objects=[lazy_object_file_record],
                        output_index_shards_directory_path=lazy_object_file_record.output_index_shards_directory_path,
                        output_premerged_bitcode_directory_path=lazy_object_file_record.output_premerged_bitcode_directory_path,
                        output_plan_file_path=lazy_object_file_record.output_plan_file_path,
                    )
            elif record["record_type"] == MetafileRecordType.EAGER.value:
                eager_object_file_record = EagerObjectFile(**record)
                object_file_records_map[
                    eager_object_file_record.input_object_file_path
                ] = eager_object_file_record
            else:
                raise Exception(f"Unknown record type: {record['record_type']}")

    return object_file_records_map, archive_records_map


def _populate_premerger_path_conversion_maps(
    object_file_records_map: dict[str, EagerObjectFile | LazyObjectFile],
) -> tuple[dict[str, str], dict[str, str]]:
    original_bitcode_to_merged_bitcode_path_mapping = {}
    merged_bitcode_to_original_bitcode_path_mapping = {}
    for (
        input_object_file_path,
        object_file_record,
    ) in object_file_records_map.items():
        output_merged_bitcode_file_path = None
        if isinstance(object_file_record, LazyObjectFile):
            assert (
                object_file_record.output_premerged_bitcode_directory_path is not None
            )
            output_merged_bitcode_file_path = os.path.join(
                object_file_record.output_premerged_bitcode_directory_path,
                input_object_file_path + MERGED_BITCODE_SUFFIX,
            )
        elif isinstance(object_file_record, EagerObjectFile):
            assert object_file_record.output_premerged_bitcode_file_path is not None
            output_merged_bitcode_file_path = (
                object_file_record.output_premerged_bitcode_file_path
            )

        original_bitcode_to_merged_bitcode_path_mapping[input_object_file_path] = (
            output_merged_bitcode_file_path
        )
        merged_bitcode_to_original_bitcode_path_mapping[
            output_merged_bitcode_file_path
        ] = input_object_file_path

    return (
        original_bitcode_to_merged_bitcode_path_mapping,
        merged_bitcode_to_original_bitcode_path_mapping,
    )


def _run_thin_link(
    premerger_enabled: bool,
    original_bitcode_to_merged_bitcode_path_mapping: dict[str, str],
    thin_link_args: list[str],
):
    if premerger_enabled:
        # Buck requires actions write output files to a location set by the build system.  When toolchain
        # binaries cannot for whatever reason easily produce output files at known locations configurable
        # locations, the usual approach is to use a wrapper Python script such as this one to place the
        # files at the locations buck expects.  In this case, this will not work as sharded summaries embed
        # paths to other bitcode files within the bitcode.  That means the toolchain must write the merged
        # bitcode files to locations buck expects in the first place. This json document is parsed by the
        # linker and communicates the locations at which the toolchain must write the merged bitcode files.
        with tempfile.NamedTemporaryFile(mode="w+t") as premerger_output_mapping:
            json.dump(
                original_bitcode_to_merged_bitcode_path_mapping,
                premerger_output_mapping,
            )
            premerger_output_mapping.flush()
            thin_link_args.append(
                f"-Wl,-mllvm,-premerger-output-map={premerger_output_mapping.name}"
            )
            subprocess.check_call(thin_link_args)
    else:
        subprocess.check_call(thin_link_args)


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("--meta")
    parser.add_argument("--index")
    parser.add_argument("--link-plan")
    parser.add_argument("--final-link-index")
    parser.add_argument("--enable-premerger", action="store_true")
    parser.add_argument("index_args", nargs=argparse.REMAINDER)
    args = parser.parse_args(argv[1:])

    premerger_enabled = args.enable_premerger

    def read_imports(imports_path):
        with open(imports_path) as infile:
            if not premerger_enabled:
                return [line.strip() for line in infile.readlines()]

            result = []
            for line in infile.readlines():
                if line.strip().endswith(".merged.bc"):
                    result.append(
                        merged_bitcode_to_original_bitcode_path_mapping[line.strip()]
                    )
                else:
                    result.append(line.strip())
            return result

    def index_path(path):
        return os.path.join(args.index, path)

    object_file_records_map, archive_records_map = _parse_meta_file_records(args.meta)

    original_bitcode_to_merged_bitcode_path_mapping = {}
    if premerger_enabled:
        (
            original_bitcode_to_merged_bitcode_path_mapping,
            merged_bitcode_to_original_bitcode_path_mapping,
        ) = _populate_premerger_path_conversion_maps(object_file_records_map)

    _run_thin_link(
        premerger_enabled,
        original_bitcode_to_merged_bitcode_path_mapping,
        args.index_args[1:],
    )

    # Thin-link will write two "index" files, one named "index" the other "index.full". We read the first to get
    # the set of bitcode files that were loaded. Notably this will not include native object files fed to the link directly.
    # This set will be used to avoid creating opt action for bitcode we don't load anyways.
    loaded_input_bitcode_files = set()
    with open(index_path("index")) as indexfile:
        for line in indexfile:
            # Bitcode files that appear in the index are prefixed with the argument to --thinlto-prefix-replace
            # which in our case is the directory index is placed in. Remove this prefix to get the original path.
            input_bitcode_file_path = os.path.relpath(line.strip(), start=args.index)
            loaded_input_bitcode_files.add(input_bitcode_file_path)

    def _input_bitcode_file_path_is_loaded_by_linker(path):
        return path in loaded_input_bitcode_files

    absorbed_source_files = set()
    non_lto_objects = {}

    # Generate plans for eager object file
    for path, data in sorted(object_file_records_map.items(), key=lambda v: v[0]):
        if isinstance(data, LazyObjectFile):
            continue

        # The linker will not write the sharded index to the location buck expects it to be by default, we need
        # to move it there.
        final_sharded_index_output_path = data.output_index_shard_file_path
        os.makedirs(os.path.dirname(final_sharded_index_output_path), exist_ok=True)

        temporary_sharded_index_location = index_path(path) + BITCODE_SUFFIX
        imports_file_path = index_path(path) + IMPORTS_SUFFIX

        # import files are only written for bitcode files
        if os.path.exists(imports_file_path):
            assert os.path.exists(temporary_sharded_index_location), (
                "missing sharded index file for %s" % path
            )
            os.rename(temporary_sharded_index_location, final_sharded_index_output_path)
            imports = read_imports(imports_file_path)
            imports_list = []
            archives_list = []
            for import_path in imports:
                imported_object_file_record = object_file_records_map[import_path]
                if isinstance(imported_object_file_record, LazyObjectFile):
                    archives_list.append(
                        int(imported_object_file_record.starlark_array_index)
                    )
                else:
                    imports_list.append(
                        imported_object_file_record.starlark_array_index
                    )
            plan = {
                "imports": imports_list,
                "archive_imports": archives_list,
                "is_bc": True,
            }

            if premerger_enabled:
                merged_bitcode_path = original_bitcode_to_merged_bitcode_path_mapping[
                    path
                ]
                assert os.path.exists(
                    merged_bitcode_path
                ), f"missing merged bitcode file at {merged_bitcode_path}"
                merge_state = read_merged_bitcode_file(merged_bitcode_path)
                if merge_state == BitcodeMergeState.ABSORBED:
                    absorbed_source_files.add(path)
                plan["merge_state"] = merge_state.value
        else:
            non_lto_objects[data.starlark_array_index] = 1
            with open(final_sharded_index_output_path, "w"):
                pass
            plan = {
                "is_bc": False,
            }

        with open(data.output_plan_file_path, "w") as planout:
            json.dump(plan, planout, sort_keys=True)

    # Generate plans for each achive
    for archive in archive_records_map.values():
        # For archives, we must produce a plan that provides Starlark enough
        # information about how to launch a dynamic opt for each object file
        # in the archive.
        archive_plan = {}

        # This is convenient to store, since it's difficult for Starlark to
        # calculate it.
        archive_plan["base_dir"] = os.path.dirname(archive.output_plan_file_path)
        object_plans = []
        output_path = archive.output_index_shards_directory_path
        os.makedirs(output_path, exist_ok=True)
        if premerger_enabled:
            merged_bitcode_output_path = archive.output_premerged_bitcode_directory_path
            os.makedirs(merged_bitcode_output_path, exist_ok=True)
        for archive_member_object_file_record in archive.objects:
            obj = archive_member_object_file_record.input_object_file_path
            imports_path = index_path(obj) + IMPORTS_SUFFIX
            if os.path.exists(imports_path):
                bc_file = index_path(obj) + BITCODE_SUFFIX
                index_path(obj) + MERGED_BITCODE_SUFFIX
                os.rename(bc_file, os.path.join(output_path, os.path.basename(bc_file)))

                imports = read_imports(imports_path)
                imports_list = []
                archives_list = []
                for import_path in imports:
                    object_file_record = object_file_records_map[import_path]
                    if isinstance(object_file_record, LazyObjectFile):
                        archives_list.append(
                            int(object_file_record.starlark_array_index)
                        )
                    else:
                        imports_list.append(object_file_record.starlark_array_index)

                object_plan = {
                    "is_bc": True,
                    "path": obj,
                    "imports": imports_list,
                    "archive_imports": archives_list,
                    "bitcode_file": os.path.join(
                        output_path, os.path.basename(bc_file)
                    ),
                }

                if not _input_bitcode_file_path_is_loaded_by_linker(obj):
                    object_plan["not_loaded_by_linker"] = True

                if premerger_enabled:
                    merged_bc_file = original_bitcode_to_merged_bitcode_path_mapping[
                        obj
                    ]
                    merge_state = read_merged_bitcode_file(merged_bc_file)
                    if merge_state == BitcodeMergeState.ABSORBED:
                        absorbed_source_files.add(obj)
                    object_plan["merge_state"] = merge_state.value
                    object_plan["merged_bitcode_path"] = merged_bc_file

                object_plans.append(object_plan)
            else:
                object_plans.append(
                    {
                        "is_bc": False,
                        "path": obj,
                        "merge_state": BitcodeMergeState.STANDALONE.value,
                    }
                )

        archive_plan["objects"] = object_plans
        with open(archive.output_plan_file_path, "w") as planout:
            json.dump(archive_plan, planout, sort_keys=True)

    # Dump the set of input object files that were already native object files instead of bitcode
    with open(args.link_plan, "w") as outfile:
        json.dump(
            {
                "non_lto_objects": non_lto_objects,
            },
            outfile,
            sort_keys=True,
        )

    # The "index.full" file is a filelist that will be used as input to the filelink, providing a list and order
    # in which to provide the native object files to the final link. However, it refers to input bitcode files by their name
    # as provided to thin-link. Opt + codegen actions will consume these bitcode files and write them elsewhere. This step takes
    # this filelist and translates input bitcode file paths to the final path where the native object files will be written.
    with open(index_path("index.full")) as full_index_input, open(
        args.final_link_index, "w"
    ) as final_link_index_output:
        for line in full_index_input:
            line = line.strip()
            path = os.path.relpath(line, start=args.index)
            # Bitcode files that have been absorbed into other bitcode files by the pre-merger need to be removed from the filelist.
            if path in absorbed_source_files:
                continue

            if path in loaded_input_bitcode_files:
                if isinstance(object_file_records_map[path], EagerObjectFile):
                    # This is a roundabout method of getting the location opt actions will write the produced native object file for an eager bitcode file. This only works because the sharded index and the output native object file are written side by side with a different suffix.
                    output = object_file_records_map[
                        path
                    ].output_index_shard_file_path.replace(
                        BITCODE_SUFFIX, OPT_OBJECTS_SUFFIX
                    )
                    final_link_index_output.write(output + "\n")
                elif isinstance(object_file_records_map[path], LazyObjectFile):
                    opt_objects_path = path.replace(
                        "/objects/", "/opt_objects/objects/"
                    )
                    final_link_index_output.write(opt_objects_path + "\n")
                else:
                    raise Exception(
                        f"unexpected object file record type: {object_file_records_map[path]}"
                    )
            else:
                # handle input files that did not come from linker input, e.g. linkerscirpts
                final_link_index_output.write(line + "\n")


sys.exit(main(sys.argv))
