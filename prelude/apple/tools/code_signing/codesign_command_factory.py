# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# pyre-strict

from abc import ABCMeta, abstractmethod
from pathlib import Path
from typing import List, Optional, Union


class ICodesignCommandFactory(metaclass=ABCMeta):
    @abstractmethod
    def codesign_command(
        self,
        path: Path,
        identity_fingerprint: str,
        entitlements: Optional[Path],
        codesign_args: List[str],
        extra_file_paths: Optional[List[Path]],
    ) -> List[Union[str, Path]]:
        raise NotImplementedError


class DefaultCodesignCommandFactory(ICodesignCommandFactory):
    codesign_tool: Path
    _command_args: List[str] = ["--force", "--sign"]

    def __init__(self, codesign_tool: Optional[Path]) -> None:
        self.codesign_tool = codesign_tool or Path("codesign")

    def codesign_command(
        self,
        path: Path,
        identity_fingerprint: str,
        entitlements: Optional[Path],
        codesign_args: List[str],
        extra_file_paths: Optional[List[Path]],
    ) -> List[Union[str, Path]]:
        if extra_file_paths:
            raise RuntimeError(
                f"Extra codesign paths unsupported for non-dry signing (path: `{path}`, extra paths: `{extra_file_paths}`)"
            )
        entitlements_args = ["--entitlements", entitlements] if entitlements else []
        return (
            [self.codesign_tool]
            + DefaultCodesignCommandFactory._command_args
            + [identity_fingerprint]
            + codesign_args
            + entitlements_args
            + [path]
        )


class DryRunCodesignCommandFactory(ICodesignCommandFactory):
    codesign_tool: Path

    def __init__(self, codesign_tool: Path) -> None:
        self.codesign_tool = codesign_tool

    def codesign_command(
        self,
        path: Path,
        identity_fingerprint: str,
        entitlements: Optional[Path],
        codesign_args: List[str],
        extra_file_paths: Optional[List[Path]],
    ) -> List[Union[str, Path]]:
        args = [path, "--identity", identity_fingerprint]
        if entitlements:
            args += ["--entitlements", entitlements] if entitlements else []
        if extra_file_paths:
            args += ["--extra-paths-to-sign"]
            args += extra_file_paths
        return [self.codesign_tool] + args
