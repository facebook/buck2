#!/usr/bin/env python3
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""
A simple wrapper around a distributed thinlto index command to fit into buck2's
distributed thinlto build.

This reads in a couple of things:
    1. The "meta" file. This is a list of tuples of (object file, index output,
    plan output). All items are line-separated (so each tuple is three lines).
    2. The index and link plan output paths
    3. The commands for the actual index command.

It will invoke the index command and then copy the index outputs to the
requested locations and write a plan for each of those objects. This "plan" is
a simple json file with the most important thing being a list of the indices
of the imports needed for that file.

It will then additionally write a link plan, which is just a translation of
the thinlto index (which lists the objects actually needed for the final link).


Both opt and link plans use indices to refer to other files because it allows the bzl
code to easily map back to other objects held in buck memory.
"""

# pyre-unsafe

import argparse
import json
import os
import os.path
import subprocess
import sys
import tempfile
from enum import Enum
from typing import List


def _get_argsfile(args) -> str:
    # go through the flags passed to linker and find the index argsfile
    argsfiles = list(
        filter(lambda arg: arg.endswith("thinlto_index_argsfile"), args.index_args)
    )
    assert (
        len(argsfiles) == 1
    ), f"expect only 1 argsfile but seeing multiple ones: {argsfiles}"
    argsfile = argsfiles[0]
    if argsfile.startswith("@"):
        argsfile = argsfile[1:]
    return argsfile


def _extract_lib_search_path(argsfile_path: str) -> List[str]:
    lib_search_path = []
    with open(argsfile_path) as argsfile:
        for line in argsfile:
            if line.startswith("-L"):
                lib_search_path.append(line.strip())
    return lib_search_path


class BitcodeMergeState(str, Enum):
    STANDALONE = "STANDALONE"
    ABSORBED = "ABSORBED"
    ROOT = "ROOT"
    NOT_LOADED = "NOT_LOADED"


LLVM_BITCODE_WRAPPER_FILE_MAGIC = 0xDEC0170B


def is_file_llvm_bitcode_wrapper_file(filepath: str) -> bool:
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

    assert False, f"unexpected merged bitcode file contents: {merged_bitcode_path}"


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("--meta")
    parser.add_argument("--index")
    parser.add_argument("--link-plan")
    parser.add_argument("--final-link-index")
    parser.add_argument("--enable-premerger", action="store_true")
    parser.add_argument("index_args", nargs=argparse.REMAINDER)
    args = parser.parse_args(argv[1:])

    bitcode_suffix = ".thinlto.bc"
    imports_suffix = ".imports"
    opt_objects_suffix = ".opt.o"  # please note the files are not exist yet, this is to generate the index file use in final link
    merged_bitcode_suffix = ".merged.bc"

    premerger_enabled = args.enable_premerger

    with open(args.meta) as meta:
        meta_lines = [line.strip() for line in meta.readlines()]

    def read_imports(imports_path):
        with open(imports_path) as infile:
            if not premerger_enabled:
                return [line.strip() for line in infile.readlines()]

            result = []
            for line in infile.readlines():
                if line.strip().endswith(".merged.bc"):
                    result.append(
                        output_merged_bitcode_file_path_to_input_bitcode_file_path_mapping[
                            line.strip()
                        ]
                    )
                else:
                    result.append(line.strip())
            return result

    def index_path(path):
        return os.path.join(args.index, path)

    # The meta file comes directly from dist_lto.bzl and consists of a list of
    # 9-tuples of information. It is easiest for us to write each tuple member
    # as a separate line in Starlark, so these 9-tuples are encoded in groups
    # of nine lines.
    #
    # The nine pieces of information are:
    #  1. The path to the source bitcode file. This is used as an index into
    #     a dictionary (`mapping`) that records much of the metadata coming
    #     from these lines.
    #  2. The path to an output bitcode file. This script is expected to place a
    #     ThinLTO index file at this location (suffixed `.thinlto.bc`).
    #  3. If the premerger is enabled, the path to a merged bitcode file.
    #     This script is expected to place a file at this location (suffixed `.merged.bc`).
    #  4. The path to an output plan. This script is expected to place a link
    #     plan here (a JSON document indicating which other object files this)
    #     object file depends on, among other things.
    #  5. The link data's index in the Starlark array.
    #  6. If this object file came from an archive, the name of the archive. Otherwise,
    #     this line is empty.
    #  7. If this object file came from an archive, the path to an output plan.
    #     This script is expected to produce an archive link plan here (a JSON)
    #     document similar to the object link plan, except containing link
    #     information for every file in the archive from which this object
    #     came. Otherwise, this line is empty.
    #  8. If this object file came from an archive, the indexes directory of that
    #     archive. This script is expected to place all ThinLTO indexes derived
    #     from object files originating from this archive in that directory.
    #     Otherwise, this line is empty.
    #  9. If this object file came from an archive, and the premerger is enabled,
    #     the merged bc directory of that archive. This script is expected to place
    #     all merged bitcode files derived from object files originating
    #     from this archive in that directory. Otherwise, this line is empty.
    #
    # There are two indices that are derived from this meta file: the object
    # index (mapping["index"]) and the archive index (mapping["archive_index"]).
    # These indices are indices into Starlark arrays for all objects and archive
    # linkables, respectively. This script does not inspect them.
    mapping = {}
    archives = {}
    input_bitcode_file_path_to_output_merged_bitcode_file_path_mapping = {}
    output_merged_bitcode_file_path_to_input_bitcode_file_path_mapping = {}
    for i in range(0, len(meta_lines), 9):
        path = meta_lines[i]
        output = meta_lines[i + 1]
        merged_output = meta_lines[i + 2]
        plan_output = meta_lines[i + 3]
        idx = int(meta_lines[i + 4])
        archive_name = meta_lines[i + 5]
        archive_plan = meta_lines[i + 6]
        archive_index_dir = meta_lines[i + 7]
        archive_merged_bitcode_dir = meta_lines[i + 8]

        archive_idx = idx if output == "" else None  # archives do not have outputs
        mapping[path] = {
            "output": output,
            "merged_output": merged_output,
            "plan_output": plan_output,
            "index": idx,
            "archive_index": archive_idx,
            "archive_name": archive_name,
        }
        if archive_idx is not None:
            if premerger_enabled:
                input_bitcode_file_path_to_output_merged_bitcode_file_path_mapping[
                    path
                ] = os.path.join(
                    archive_merged_bitcode_dir, path + merged_bitcode_suffix
                )
                output_merged_bitcode_file_path_to_input_bitcode_file_path_mapping[
                    os.path.join(
                        archive_merged_bitcode_dir, path + merged_bitcode_suffix
                    )
                ] = path
            archives[idx] = {
                "name": archive_name,
                "objects": [],
                "plan": archive_plan,
                "index_dir": archive_index_dir,
                "merged_bitcode_dir": archive_merged_bitcode_dir,
            }
        elif premerger_enabled:
            input_bitcode_file_path_to_output_merged_bitcode_file_path_mapping[path] = (
                merged_output
            )
            output_merged_bitcode_file_path_to_input_bitcode_file_path_mapping[
                merged_output
            ] = path

    if premerger_enabled:
        with tempfile.NamedTemporaryFile(mode="w+t") as premerger_output_mapping:
            json.dump(
                input_bitcode_file_path_to_output_merged_bitcode_file_path_mapping,
                premerger_output_mapping,
            )
            premerger_output_mapping.flush()
            thin_link_args = args.index_args[1:]
            thin_link_args.append(
                f"-Wl,-mllvm,-premerger-output-map={premerger_output_mapping.name}"
            )
            subprocess.check_call(thin_link_args)
    else:
        subprocess.check_call(args.index_args[1:])

    # We read the `index`` and `index.full`` files produced by linker in index stage
    # and translate them to 2 outputs:
    # 1. A link plan build final_link args. (This one may be able to be removed if we refactor the workflow)
    # 2. A files list (*.final_link_index) used for final link stage which includes all the
    #    files needed. it's based on index.full with some modification, like path updates
    #    and redundant(added by toolchain) dependencies removing.
    index = {}
    index_files_set = set()
    loaded_input_bitcode_files = set()
    absorbed_source_files = set()
    with open(index_path("index")) as indexfile:
        for line in indexfile:
            line = line.strip()
            index_files_set.add(line)
            path = os.path.relpath(line, start=args.index)
            loaded_input_bitcode_files.add(path)
            index[mapping[path]["index"]] = 1

    def _input_bitcode_file_path_is_loaded_by_linker(path):
        return path in loaded_input_bitcode_files

    non_lto_objects = {}
    for path, data in sorted(mapping.items(), key=lambda v: v[0]):
        output_loc = data["output"]
        if os.path.exists(output_loc):
            continue

        if data["archive_index"] is not None:
            archives[data["archive_index"]]["objects"].append(path)
            continue

        bc_file = index_path(path) + bitcode_suffix
        imports_path = index_path(path) + imports_suffix
        os.makedirs(os.path.dirname(output_loc), exist_ok=True)

        if os.path.exists(imports_path):
            assert os.path.exists(bc_file), "missing bc file for %s" % path
            os.rename(bc_file, output_loc)
            imports = read_imports(imports_path)
            imports_list = []
            archives_list = []
            for path in imports:
                entry = mapping[path]
                if entry["archive_index"] is not None:
                    archives_list.append(int(entry["archive_index"]))
                else:
                    imports_list.append(entry["index"])
            plan = {
                "imports": imports_list,
                "archive_imports": archives_list,
                "index": data["index"],
                "bitcode_file": bc_file,
                "path": path,
                "is_bc": True,
            }

            if premerger_enabled:
                merged_bitcode_path = (
                    input_bitcode_file_path_to_output_merged_bitcode_file_path_mapping[
                        path
                    ]
                )
                assert os.path.exists(
                    merged_bitcode_path
                ), f"missing merged bitcode file at {merged_bitcode_path}"
                merge_state = read_merged_bitcode_file(merged_bitcode_path)
                if merge_state == BitcodeMergeState.ABSORBED:
                    absorbed_source_files.add(path)
                plan["merge_state"] = merge_state.value
        else:
            non_lto_objects[data["index"]] = 1
            with open(output_loc, "w"):
                pass
            plan = {
                "is_bc": False,
            }

        with open(data["plan_output"], "w") as planout:
            json.dump(plan, planout, sort_keys=True)

    for archive in archives.values():
        # For archives, we must produce a plan that provides Starlark enough
        # information about how to launch a dynamic opt for each object file
        # in the archive.
        archive_plan = {}

        # This is convenient to store, since it's difficult for Starlark to
        # calculate it.
        archive_plan["base_dir"] = os.path.dirname(archive["plan"])
        object_plans = []
        output_path = archive["index_dir"]
        os.makedirs(output_path, exist_ok=True)
        if premerger_enabled:
            merged_bitcode_output_path = archive["merged_bitcode_dir"]
            os.makedirs(merged_bitcode_output_path, exist_ok=True)
        for obj in archive["objects"]:
            imports_path = index_path(obj) + imports_suffix
            if os.path.exists(imports_path):
                bc_file = index_path(obj) + bitcode_suffix
                index_path(obj) + merged_bitcode_suffix
                os.rename(bc_file, os.path.join(output_path, os.path.basename(bc_file)))

                imports = read_imports(imports_path)
                imports_list = []
                archives_list = []
                for path in imports:
                    entry = mapping[path]
                    if entry["archive_index"] is not None:
                        archives_list.append(int(entry["archive_index"]))
                    else:
                        imports_list.append(entry["index"])

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
                    merged_bc_file = input_bitcode_file_path_to_output_merged_bitcode_file_path_mapping[
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
        with open(archive["plan"], "w") as planout:
            json.dump(archive_plan, planout, sort_keys=True)

    with open(args.link_plan, "w") as outfile:
        json.dump(
            {
                "non_lto_objects": non_lto_objects,
                "index": index,
            },
            outfile,
            indent=2,
            sort_keys=True,
        )

    # Append all search path flags (e.g -Lfbcode/third-party-buck/platform010/build/glibc/lib) from argsfile to final_index
    # this workaround is to make dist_lto compatible with link_group. see T136415235 for more info
    argsfile = _get_argsfile(args)
    lib_search_path = _extract_lib_search_path(argsfile)

    # build index file for final link use
    with open(index_path("index.full")) as full_index_input, open(
        args.final_link_index, "w"
    ) as final_link_index_output:
        final_link_index_output.write("\n".join(lib_search_path) + "\n")
        for line in full_index_input:
            line = line.strip()
            path = os.path.relpath(line, start=args.index)
            if path in absorbed_source_files:
                continue
            if line in index_files_set:
                if mapping[path]["output"]:
                    # handle files that were not extracted from archives
                    output = mapping[path]["output"].replace(
                        bitcode_suffix, opt_objects_suffix
                    )
                    final_link_index_output.write(output + "\n")
                elif os.path.exists(index_path(path) + imports_suffix):
                    # handle files built from source that were extracted from archives
                    opt_objects_path = path.replace(
                        "/objects/", "/opt_objects/objects/"
                    )
                    final_link_index_output.write(opt_objects_path + "\n")
                else:
                    # handle pre-built archives
                    final_link_index_output.write(line + "\n")
            else:
                # handle input files that did not come from linker input, e.g. linkerscirpts
                final_link_index_output.write(line + "\n")


sys.exit(main(sys.argv))
