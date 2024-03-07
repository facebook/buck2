# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# pyre-strict

import logging
import os
import shlex
import shutil
import subprocess
import tempfile
from dataclasses import dataclass
from pathlib import Path
from typing import List, Optional, Union

_LOGGER: logging.Logger = logging.getLogger(__name__)


@dataclass
class SwiftSupportArguments:
    swift_stdlib_command: str
    binary_destination: Path
    appclips_destination: Path
    frameworks_destination: Path
    plugins_destination: Path
    platform: str
    sdk_root: Path


def run_swift_stdlib_tool(bundle_path: Path, args: SwiftSupportArguments) -> List[Path]:
    # TODO(T181556849) when incremental bundling is on, binary, frameworks and plugins are not changed, signing identity is unchanged skip this step.
    bundle_relative_output_paths = []
    with tempfile.TemporaryDirectory() as tmp_dir:
        # When signing, swift-stdlib-tool needs a proper PATH environment variable.
        # Assume the current environment has it already.
        env = os.environ.copy()
        # xcrun doesn't like relative paths
        env["SDKROOT"] = os.path.abspath(args.sdk_root)
        cmd = _execution_command(bundle_path, args, tmp_dir)
        _LOGGER.info(
            f"Running Swift stdlib tool with command: `{cmd}` and environment `{env}`."
        )
        result = subprocess.run(cmd, env=env)
        result.check_returncode()
        outputs = sorted(os.listdir(tmp_dir))
        frameworks_path = bundle_path / args.frameworks_destination
        if outputs:
            frameworks_path.mkdir(exist_ok=True)
        for output in outputs:
            shutil.move(os.path.join(tmp_dir, output), frameworks_path)
        bundle_relative_output_paths = [
            args.frameworks_destination / o for o in outputs
        ]
    return bundle_relative_output_paths


def _execution_command(
    bundle_path: Path,
    args: SwiftSupportArguments,
    tmp_dir: str,
) -> List[Union[str, Path]]:
    return shlex.split(args.swift_stdlib_command) + [
        "--copy",
        "--strip-bitcode",
        "--scan-executable",
        bundle_path / args.binary_destination,
        "--scan-executable",
        bundle_path / args.appclips_destination,
        "--scan-folder",
        bundle_path / args.frameworks_destination,
        "--scan-folder",
        bundle_path / args.plugins_destination,
        "--destination",
        tmp_dir,
        "--platform",
        args.platform,
    ]
