#!/usr/bin/env fbpython
# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

import json
import logging
import os
import shutil
import subprocess
import sys


MODE_CONFIGS_DIR = "mode_configs"


def is_explicit_target(target):
    return "..." not in target and not target.endswith(":")


def is_target_alias(target):
    return all(symbol not in target for symbol in [":", "/", "..."])


def get_mode_hashes(
    sample_target, explicit_targets, mode_files, extra_buck_options, debug
):
    # Resolve sample_target to full target label to match the BXL script output.
    sample_target = subprocess.check_output(
        ["buck2", "targets", sample_target],
        text=True,
    ).strip()

    mode_hashes = {}
    default_mode_file = mode_files[0]
    for mode_file in mode_files:
        bxl_cmds = [
            "buck2",
        ]
        if mode_file != default_mode_file:
            bxl_cmds += ["--isolation-dir", "vsgo-" + mode_file.split("/")[-1]]
        bxl_cmds += (
            [
                "bxl",
                "@" + mode_file,
                "prelude//ide_integrations/visual_studio/get_mode_hashes.bxl:main",
            ]
            + extra_buck_options
            + ["--", "--target", sample_target]
            + explicit_targets
        )

        if debug:
            print("Running bxl command:", " ".join(bxl_cmds))
        try:
            output = json.loads(
                subprocess.check_output(
                    bxl_cmds,
                    text=True,
                    stderr=subprocess.PIPE,
                )
            )
            # Change the key 'sample_target' to 'default' in the output JSON
            if sample_target not in output:
                raise Exception(
                    "BXL script did not return sample_target in the output JSON"
                )
            output["default"] = output.pop(sample_target)

        except subprocess.CalledProcessError as e:
            print("\nstdout:\n" + e.stdout, file=sys.stderr)
            print("\nstderr:\n" + e.stderr, file=sys.stderr)
            raise

        mode_hashes[mode_file] = output

    return mode_hashes


# Replace option prefix "--" with "\-\-" to avoid BXL mistakes them as arguments to BXL.
def _escape_arg(arg):
    if arg.startswith("--"):
        return r"\-\-" + arg[2:]
    if arg.startswith("-"):
        return r"\-" + arg[1:]
    return arg


def gen_mode_configs(bxl_path, mode_files, fbsource, debug):
    mode_config_paths = []
    default_mode_file = mode_files[0]
    for mode_file in mode_files:
        bxl_cmds = [
            "buck2",
        ]
        if mode_file != default_mode_file:
            bxl_cmds += ["--isolation-dir", "vsgo-" + mode_file.split("/")[-1]]
        bxl_cmds += [
            "bxl",
            "@" + mode_file,
            os.path.dirname(bxl_path) + "/gen_mode_configs.bxl:main",
            "--",
            "--mode_name",
            mode_file,
            "--fbsource",
            str(fbsource).lower(),
        ]
        if debug:
            print("Running BXL command:", " ".join(bxl_cmds))
        try:
            bxl_process = subprocess.run(
                bxl_cmds, text=True, capture_output=True, check=True
            )
        except subprocess.CalledProcessError as e:
            print("\nstdout:\n" + e.stdout, file=sys.stderr)
            print("\nstderr:\n" + e.stderr, file=sys.stderr)
            raise
        mode_config_paths.append(bxl_process.stdout.strip())
    return mode_config_paths


def main(
    targets,
    mode_files,
    extra_bxl_options,
    extra_buck_options,
    generated_folder,
    recursive_target_types,
    target_exclude_patterns,
    target_include_patterns,
    solution_name,
    startup_target,
    fbsource,
    bxl_path,
    sample_target,
    debug,
):
    mode_configs = gen_mode_configs(bxl_path, mode_files, fbsource, debug)

    if len(mode_files) > 1:
        explicit_targets = [target for target in targets if is_explicit_target(target)]
        mode_hashes = get_mode_hashes(
            sample_target,
            explicit_targets,
            mode_files,
            extra_bxl_options,
            debug,
        )
    else:
        mode_hashes = {}

    if debug:
        print("mode_hashes:", mode_hashes)

    default_mode_file = mode_files[0]
    bxl_cmds = (
        [
            "buck2",
            "bxl",
            "@" + default_mode_file,
        ]
        + extra_bxl_options
        + [
            bxl_path + ":main",
            "--",
        ]
        + ["--target"]
        + targets
        + ["--mode_files"]
        + mode_files
    )
    if extra_buck_options:
        # Pass extra buck options verbatim so that run/debug invokes buck using the same options beside target and mode file.
        bxl_cmds += ["--extra_buck_options"] + [
            _escape_arg(o) for o in extra_buck_options
        ]
    bxl_cmds += ["--mode_hashes", json.dumps(mode_hashes, separators=(",", ":"))]
    if recursive_target_types:
        bxl_cmds += ["--recursive_target_types"] + recursive_target_types
    if target_exclude_patterns:
        bxl_cmds += ["--target_exclude_pattern"] + target_exclude_patterns
    if target_include_patterns:
        bxl_cmds += ["--target_include_pattern"] + target_include_patterns
    if solution_name:
        bxl_cmds += ["--solution_name", solution_name]
    if startup_target:
        bxl_cmds += ["--startup_target", startup_target]
    bxl_cmds += ["--output", "json"]
    bxl_cmds += ["--fbsource", str(fbsource).lower()]
    if debug:
        bxl_cmds += ["--log_level", "0"]
        print("Running BXL command:", " ".join(bxl_cmds))

    # Capture both stdout and stderr to "tee" to both console and remote error logs.
    try:
        bxl_process = subprocess.run(
            bxl_cmds, text=True, capture_output=True, check=True
        )
    except subprocess.CalledProcessError as e:
        print("\nstdout:\n" + e.stdout, file=sys.stderr)
        print("\nstderr:\n" + e.stderr, file=sys.stderr)
        raise
    # Show potential warnings to user.
    print(bxl_process.stderr, file=sys.stderr)
    bxl_output = json.loads(bxl_process.stdout)
    sln_path = bxl_output["sln_path"]

    parts = sln_path.split(os.sep)
    gen_root_index = parts.index("main.bxl") + 1
    gen_root = os.sep.join(parts[: gen_root_index + 1])

    os.makedirs(os.path.join(gen_root, MODE_CONFIGS_DIR), exist_ok=True)
    for file_path in mode_configs:
        file_name = os.path.basename(file_path)
        destination_file_path = os.path.join(gen_root, MODE_CONFIGS_DIR, file_name)
        shutil.copy(file_path, destination_file_path)

    logging.warning(
        "Copying generated project and solution files to final destination ..."
    )
    shutil.copytree(gen_root, generated_folder, dirs_exist_ok=True)
    sln_relative_to_gen_root = os.sep.join(parts[gen_root_index + 1 :])
    sln_path = os.path.normcase(
        os.path.join(generated_folder, sln_relative_to_gen_root)
    )
    dest_sln_path = os.path.join(generated_folder, os.path.basename(sln_path))
    shutil.move(sln_path, dest_sln_path)

    return bxl_output | {"sln_path": dest_sln_path}


DEFAULT_RECURSIVE_TARGET_TYPES = [
    "cxx_binary",
    "cxx_library",
    "cxx_test",
    "alias",
    "command_alias",
]

if __name__ == "__main__":
    import argparse

    parser = argparse.ArgumentParser(
        prog="vsgo",
        description="Visual Studio project generator for buck targets",
    )
    parser.add_argument(
        "targets",
        help="""List of buck targets, aliases and/or patterns. Dependencies and transitive dependencies are automatically pulled in.
Individual specified targets are preferred over target patterns as the latter will usually pull in unused targets which
could potentially slow down project generation and project loading significantly. Examples:
    1. Single fully-specified target:
        //arvr/projects/pcsdk:OVRServer
        (Only fully-specified targets are guaranteed to be able to link to the debugger)
    2. target alias:
        ovrserver
    3. '...' target pattern (CAUTION: Using '...' in targets is not properly supported and may lead to incorrect behavior. Please consider
using explicit targets instead. It also might bring in massive targets with recursive dependencies and result in very slow solution
generating and loading):
        //arvr/projects/mixedreality/...
    4. Combinations of above:
        ovrserver //arvr/projects/pcsdk:OVRServer
    """,
        nargs="*",
        type=str,
        default=[],
    )
    parser.add_argument(
        "--target",
        nargs="+",
        help="alias for positional argument targets. Preferred over positional arguments",
        default=[],
    )
    parser.add_argument(
        "--mode_files",
        nargs="+",
        help="list of mode files (without leading `@`), which will be generated as different configurations",
        default=[],
    )
    parser.add_argument(
        "--extra_bxl_options",
        nargs="+",
        help="extra options when running BXL scripts during project generation",
        default=[],
    )
    parser.add_argument(
        "--extra_buck_options",
        nargs="+",
        help="extra options when running buck from generated project settings. Note '-' within option value needs to be escaped, e.g., `vsgo //third-party/semver:basic_example --extra_buck_options '\\-\\-out' 'C:\\open\\temp-out' '\\-\\-local-only'`",
        default=[],
    )
    parser.add_argument(
        "--generated_folder",
        action="store",
        help="output directory of generated projects and solutions",
        default="generated_projects",
    )
    parser.add_argument(
        "--recursive_target_types",
        nargs="+",
        help="the types of targets that will be recovered when target patterns ('...') are expanded and transitive dependencies are queried. Default to "
        + str(DEFAULT_RECURSIVE_TARGET_TYPES),
        default=DEFAULT_RECURSIVE_TARGET_TYPES,
    )
    parser.add_argument(
        "--target_include_patterns",
        nargs="+",
        help="include target(s) only if it matches buck target pattern",
        default=[],
    )
    parser.add_argument(
        "--target_exclude_patterns",
        nargs="+",
        help="exclude target(s) if it matches buck target pattern",
        default=[],
    )
    parser.add_argument(
        "--solution_name",
        action="store",
        help="name of the generated solution",
    )
    parser.add_argument(
        "--startup_target",
        action="store",
        help="buck target to be set as the default startup project",
    )
    parser.add_argument(
        "--fbsource",
        action="store_true",
        help="whether to turn on fbsource specific behaviors",
        default=False,
    )
    parser.add_argument(
        "--bxl_path",
        action="store",
        help="path of BXL script",
        default="prelude//ide_integrations/visual_studio/main.bxl",
    )
    parser.add_argument(
        "--sample_target",
        action="store",
        help="sample target to build in order to get configuration hash values",
    )
    parser.add_argument(
        "--debug",
        action="store_true",
        help="verbose output",
        default=False,
    )

    args = parser.parse_args()
    args.targets.extend(args.target)
    if not args.targets:
        parser.error(
            "Required argument targets not found! Specify targets using either preferred `--target` option or as positional arguments."
        )
    if not args.mode_files:
        parser.error("Required argument mode_files not found!")

    bxl_output = main(
        targets=args.targets,
        mode_files=args.mode_files,
        extra_bxl_options=args.extra_bxl_options,
        extra_buck_options=args.extra_buck_options,
        generated_folder=args.generated_folder,
        recursive_target_types=args.recursive_target_types,
        target_exclude_patterns=args.target_exclude_patterns,
        target_include_patterns=args.target_include_patterns,
        solution_name=args.solution_name,
        startup_target=args.startup_target,
        fbsource=args.fbsource,
        bxl_path=args.bxl_path,
        sample_target=args.sample_target,
        debug=args.debug,
    )
    print("Solution generated at " + bxl_output["sln_path"])
