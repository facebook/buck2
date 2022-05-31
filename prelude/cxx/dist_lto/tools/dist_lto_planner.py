#!/usr/bin/env python3

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


def main(argv):
    parser = argparse.ArgumentParser()
    parser.add_argument("--meta")
    parser.add_argument("--index")
    parser.add_argument("--link-plan")
    parser.add_argument("index_args", nargs=argparse.REMAINDER)
    args = parser.parse_args(argv[1:])

    subprocess.check_call(args.index_args[1:])

    bitcode_suffix = ".thinlto.bc"
    imports_suffix = ".imports"

    with open(args.meta) as meta:
        meta_lines = [line.strip() for line in meta.readlines()]

    def read_imports(path, imports_path):
        with open(imports_path) as infile:
            return [line.strip() for line in infile.readlines()]

    def index_path(path):
        return os.path.join(args.index, path)

    # The meta file comes directly from dist_lto.bzl and consists of a list of
    # 7-tuples of information. It is easiest for us to write each tuple member
    # as a separate line in Starlark, so these 7-tuples are encoded in groups
    # of seven lines.
    #
    # The seven pieces of information are:
    #  1. The path to the source bitcode file. This is used as an index into
    #     a dictionary (`mapping`) that records much of the metadata coming
    #     from these lines.
    #  2. The path to an output bitcode file. This script is expected to place a
    #     ThinLTO index file at this location (suffixed `.thinlto.bc`).
    #  3. The path to an output plan. This script is expected to place a link
    #     plan here (a JSON document indicating which other object files this)
    #     object file depends on, among other things.
    #  4. If this object file came from an archive, the index of the archive in
    #     the Starlark archives array. Otherwise, this line is empty.
    #  5. If this object file came from an archive, the name of the archive. Otherwise,
    #     this line is empty.
    #  6. If this object file came from an archive, the path to an output plan.
    #     This script is expected to produce an archive link plan here (a JSON)
    #     document similar to the object link plan, except containing link
    #     information for every file in the archive from which this object
    #     came. Otherwise, this line is empty.
    #  7. If this object file came from an archive, the indexes directory of that
    #     archive. This script is expected to place all ThinLTO indexes derived
    #     from object files originating from this archive in that directory.
    #     Otherwise, this line is empty.
    #
    # There are two indices that are derived from this meta file: the object
    # index (mapping["index"]) and the archive index (mapping["archive_index"]).
    # These indices are indices into Starlark arrays for all objects and archive
    # linkables, respectively. This script does not inspect them.
    mapping = {}
    archives = {}
    for i in range(0, len(meta_lines), 7):
        path = meta_lines[i]
        archive_idx = meta_lines[i + 3]
        archive_name = meta_lines[i + 4]
        archive_plan = meta_lines[i + 5]
        archive_index_dir = meta_lines[i + 6]
        mapping[path] = {
            "output": meta_lines[i + 1],
            "plan_output": meta_lines[i + 2],
            "index": i // 7,
            "archive_index": archive_idx,
            "archive_name": archive_name,
        }

        if archive_idx != "":
            archives[archive_idx] = {
                "name": archive_name,
                "objects": [],
                "plan": archive_plan,
                "index_dir": archive_index_dir,
            }

    non_lto_objects = {}
    for path, data in sorted(mapping.items(), key=lambda v: v[0]):
        output_loc = data["output"]
        if os.path.exists(output_loc):
            continue

        if data["archive_index"] != "":
            # Coming from an archive - we'll handle this separately. Record it and move on.
            archives[data["archive_index"]]["objects"].append(path)
            continue

        bc_file = index_path(path) + bitcode_suffix
        imports_path = index_path(path) + imports_suffix
        os.makedirs(os.path.dirname(output_loc), exist_ok=True)

        if os.path.exists(imports_path):
            assert os.path.exists(bc_file), "missing bc file for %s" % path
            os.rename(bc_file, output_loc)
            imports = read_imports(path, imports_path)
            imports_list = []
            archives_list = []
            for path in imports:
                entry = mapping[path]
                if entry["archive_index"] != "":
                    archives_list.append(int(entry["archive_index"]))
                else:
                    imports_list.append(entry["index"])
            plan = {
                "imports": imports_list,
                "archive_imports": archives_list,
                "index": data["index"],
                "bitcode_file": bc_file,
                "path": path,
                "is_lto": True,
            }
        else:
            non_lto_objects[data["index"]] = 1
            with open(output_loc, "w"):
                pass
            plan = {
                "is_lto": False,
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
        for obj in archive["objects"]:
            imports_path = index_path(obj) + imports_suffix
            output_path = archive["index_dir"]
            os.makedirs(output_path, exist_ok=True)
            if os.path.exists(imports_path):
                bc_file = index_path(obj) + bitcode_suffix
                os.rename(bc_file, os.path.join(output_path, os.path.basename(bc_file)))
                imports = read_imports(path, imports_path)

                imports_list = []
                archives_list = []
                for path in imports:
                    entry = mapping[path]
                    if entry["archive_index"] != "":
                        archives_list.append(int(entry["archive_index"]))
                    else:
                        imports_list.append(entry["index"])
                object_plans.append(
                    {
                        "is_lto": True,
                        "path": obj,
                        "imports": imports_list,
                        "archive_imports": archives_list,
                        "bitcode_file": os.path.join(
                            output_path, os.path.basename(bc_file)
                        ),
                    }
                )
            else:
                object_plans.append(
                    {
                        "is_lto": False,
                        "path": obj,
                    }
                )

        archive_plan["objects"] = object_plans
        with open(archive["plan"], "w") as planout:
            json.dump(archive_plan, planout, sort_keys=True)

    index = {}
    with open(index_path("index")) as indexfile:
        for line in indexfile.readlines():
            line = line.strip()
            path = os.path.relpath(line, start=args.index)
            index[mapping[path]["index"]] = 1

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


sys.exit(main(sys.argv))
