# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

import os
import tempfile
import unittest
from pathlib import Path
from typing import Generator

from apple.tools.code_signing.codesign_bundle import CodesignConfiguration

from .assemble_bundle_types import BundleSpecItem
from .incremental_state import CodesignedOnCopy, IncrementalState, IncrementalStateItem
from .incremental_utils import (
    calculate_incremental_state,
    IncrementalContext,
    should_assemble_incrementally,
)

try:
    from contextlib import chdir  # pyre-ignore[21], Python 3.11+
except ImportError:
    from contextlib import contextmanager

    @contextmanager
    def chdir(path: os.PathLike) -> Generator[None, None, None]:
        cwd = os.getcwd()
        try:
            os.chdir(path)
            yield
        finally:
            os.chdir(cwd)


class TestIncrementalUtils(unittest.TestCase):
    maxDiff = None

    def test_not_run_incrementally_when_previous_build_not_incremental(self):
        spec = [
            BundleSpecItem(
                src="src/foo",
                dst="foo",
                codesign_on_copy=False,
            )
        ]
        incremental_context = IncrementalContext(
            metadata={Path("foo"): "digest"},
            state=None,
            codesigned=False,
            codesign_configuration=None,
            codesign_identity=None,
            codesign_arguments=[],
            versioned_if_macos=True,
        )
        self.assertFalse(should_assemble_incrementally(spec, incremental_context))

    def test_run_incrementally_when_previous_build_not_codesigned(self):
        spec = [
            BundleSpecItem(
                src="src/foo",
                dst="foo",
                codesign_on_copy=False,
            )
        ]
        incremental_context = IncrementalContext(
            metadata={Path("foo"): "digest"},
            state=IncrementalState(
                items=[
                    IncrementalStateItem(
                        source=Path("src/foo"),
                        destination_relative_to_bundle=Path("foo"),
                        digest="digest",
                        resolved_symlink=None,
                    )
                ],
                codesigned=False,
                codesign_configuration=None,
                codesigned_on_copy=[],
                codesign_identity=None,
                codesign_arguments=[],
                versioned_if_macos=True,
                swift_stdlib_paths=[],
            ),
            codesigned=True,
            codesign_configuration=None,
            codesign_identity=None,
            codesign_arguments=[],
            versioned_if_macos=True,
        )
        self.assertTrue(should_assemble_incrementally(spec, incremental_context))

    def test_not_run_incrementally_when_previous_build_codesigned_and_current_is_not(
        self,
    ):
        spec = [
            BundleSpecItem(
                src="src/foo",
                dst="foo",
                codesign_on_copy=False,
            )
        ]
        incremental_context = IncrementalContext(
            metadata={Path("foo"): "digest"},
            state=IncrementalState(
                items=[
                    IncrementalStateItem(
                        source=Path("src/foo"),
                        destination_relative_to_bundle=Path("foo"),
                        digest="digest",
                        resolved_symlink=None,
                    )
                ],
                codesigned=True,
                codesign_configuration=None,
                codesigned_on_copy=[],
                codesign_identity=None,
                codesign_arguments=[],
                versioned_if_macos=True,
                swift_stdlib_paths=[],
            ),
            codesigned=False,
            codesign_configuration=None,
            codesign_identity=None,
            codesign_arguments=[],
            versioned_if_macos=True,
        )
        self.assertFalse(should_assemble_incrementally(spec, incremental_context))
        # Check that behavior changes when both builds are codesigned
        incremental_context.codesigned = True
        self.assertTrue(should_assemble_incrementally(spec, incremental_context))

    def test_not_run_incrementally_when_previous_build_codesigned_with_different_identity(
        self,
    ):
        spec = [
            BundleSpecItem(
                src="src/foo",
                dst="foo",
                codesign_on_copy=False,
            )
        ]
        incremental_context = IncrementalContext(
            metadata={Path("foo"): "digest"},
            state=IncrementalState(
                items=[
                    IncrementalStateItem(
                        source=Path("src/foo"),
                        destination_relative_to_bundle=Path("foo"),
                        digest="digest",
                        resolved_symlink=None,
                    )
                ],
                codesigned=True,
                codesign_configuration=None,
                codesigned_on_copy=[],
                codesign_identity="old_identity",
                codesign_arguments=[],
                versioned_if_macos=True,
                swift_stdlib_paths=[],
            ),
            codesigned=True,
            codesign_configuration=None,
            codesign_identity="new_identity",
            codesign_arguments=[],
            versioned_if_macos=True,
        )
        self.assertFalse(should_assemble_incrementally(spec, incremental_context))
        # Check that behavior changes when identities are same
        incremental_context.state.codesign_identity = "same_identity"
        incremental_context.codesign_identity = "same_identity"
        self.assertTrue(should_assemble_incrementally(spec, incremental_context))

    def test_run_incrementally_when_codesign_on_copy_paths_match(self):
        spec = [
            BundleSpecItem(
                src="src/foo",
                dst="foo",
                codesign_on_copy=True,
            ),
            BundleSpecItem(
                src="src/bar",
                dst="bar",
                codesign_on_copy=True,
            ),
            BundleSpecItem(
                src="src/baz",
                dst="baz",
                codesign_on_copy=True,
                codesign_entitlements="entitlements.plist",
            ),
        ]
        incremental_context = IncrementalContext(
            metadata={
                Path("src/foo"): "digest",
                Path("src/baz"): "digest2",
                Path("entitlements.plist"): "entitlements_digest",
            },
            state=IncrementalState(
                items=[
                    IncrementalStateItem(
                        source=Path("src/foo"),
                        destination_relative_to_bundle=Path("foo"),
                        digest="digest",
                        resolved_symlink=None,
                    ),
                    IncrementalStateItem(
                        source=Path("src/baz"),
                        destination_relative_to_bundle=Path("baz"),
                        digest="digest2",
                        resolved_symlink=None,
                    ),
                ],
                codesigned=True,
                codesign_configuration=None,
                codesigned_on_copy=[
                    CodesignedOnCopy(
                        path=Path("foo"),
                        entitlements_digest=None,
                        codesign_flags_override=None,
                    ),
                    CodesignedOnCopy(
                        path=Path("baz"),
                        entitlements_digest="entitlements_digest",
                        codesign_flags_override=None,
                    ),
                ],
                codesign_identity="same_identity",
                codesign_arguments=[],
                versioned_if_macos=True,
                swift_stdlib_paths=[],
            ),
            codesigned=True,
            codesign_configuration=None,
            codesign_identity="same_identity",
            codesign_arguments=[],
            versioned_if_macos=True,
        )
        self.assertTrue(should_assemble_incrementally(spec, incremental_context))

    def test_not_run_incrementally_when_codesign_on_copy_paths_mismatch(self):
        spec = [
            BundleSpecItem(
                src="src/foo",
                dst="foo",
                # want it to be not codesigned in new build
                codesign_on_copy=False,
            )
        ]
        incremental_context = IncrementalContext(
            metadata={Path("src/foo"): "digest"},
            state=IncrementalState(
                items=[
                    IncrementalStateItem(
                        source=Path("src/foo"),
                        destination_relative_to_bundle=Path("foo"),
                        digest="digest",
                        resolved_symlink=None,
                    )
                ],
                codesigned=True,
                codesign_configuration=None,
                # but it was codesigned in old build
                codesigned_on_copy=[
                    CodesignedOnCopy(
                        path=Path("foo"),
                        entitlements_digest=None,
                        codesign_flags_override=None,
                    )
                ],
                codesign_identity="same_identity",
                codesign_arguments=[],
                versioned_if_macos=True,
                swift_stdlib_paths=[],
            ),
            codesigned=True,
            codesign_configuration=None,
            codesign_identity="same_identity",
            codesign_arguments=[],
            versioned_if_macos=True,
        )
        self.assertFalse(should_assemble_incrementally(spec, incremental_context))
        spec[0].codesign_on_copy = True
        self.assertTrue(should_assemble_incrementally(spec, incremental_context))

    def test_not_run_incrementally_when_codesign_on_copy_entitlements_mismatch(self):
        spec = [
            BundleSpecItem(
                src="src/foo",
                dst="foo",
                codesign_on_copy=True,
                codesign_entitlements="baz/entitlements.plist",
            )
        ]
        incremental_context = IncrementalContext(
            metadata={
                Path("src/foo"): "digest",
                Path("baz/entitlements.plist"): "new_digest",
            },
            state=IncrementalState(
                items=[
                    IncrementalStateItem(
                        source=Path("src/foo"),
                        destination_relative_to_bundle=Path("foo"),
                        digest="digest",
                        resolved_symlink=None,
                    )
                ],
                codesigned=True,
                codesign_configuration=None,
                codesigned_on_copy=[
                    CodesignedOnCopy(
                        path=Path("foo"),
                        entitlements_digest="old_digest",
                        codesign_flags_override=None,
                    )
                ],
                codesign_identity="same_identity",
                codesign_arguments=[],
                versioned_if_macos=True,
                swift_stdlib_paths=[],
            ),
            codesigned=True,
            codesign_configuration=None,
            codesign_identity="same_identity",
            codesign_arguments=[],
            versioned_if_macos=True,
        )
        self.assertFalse(should_assemble_incrementally(spec, incremental_context))
        incremental_context.metadata[Path("baz/entitlements.plist")] = "old_digest"
        self.assertTrue(should_assemble_incrementally(spec, incremental_context))

    def test_not_run_incrementally_when_codesign_on_copy_flags_mismatch(self):
        spec = [
            BundleSpecItem(
                src="src/foo",
                dst="foo",
                codesign_on_copy=True,
                codesign_flags_override=["--force"],
            )
        ]
        incremental_context = IncrementalContext(
            metadata={
                Path("src/foo"): "digest",
            },
            state=IncrementalState(
                items=[
                    IncrementalStateItem(
                        source=Path("src/foo"),
                        destination_relative_to_bundle=Path("foo"),
                        digest="digest",
                        resolved_symlink=None,
                    )
                ],
                codesigned=True,
                codesign_configuration=None,
                codesigned_on_copy=[
                    CodesignedOnCopy(
                        path=Path("foo"),
                        entitlements_digest=None,
                        codesign_flags_override=["--force", "--deep"],
                    )
                ],
                codesign_identity="same_identity",
                codesign_arguments=[],
                versioned_if_macos=True,
                swift_stdlib_paths=[],
            ),
            codesigned=True,
            codesign_configuration=None,
            codesign_identity="same_identity",
            codesign_arguments=[],
            versioned_if_macos=True,
        )
        self.assertFalse(should_assemble_incrementally(spec, incremental_context))
        incremental_context.state.codesigned_on_copy[0].codesign_flags_override = [
            "--force"
        ]
        self.assertTrue(should_assemble_incrementally(spec, incremental_context))

    def test_not_run_incrementally_when_codesign_arguments_mismatch(self):
        spec = [
            BundleSpecItem(
                src="src/foo",
                dst="foo",
            )
        ]
        incremental_context = IncrementalContext(
            metadata={
                Path("src/foo"): "digest",
            },
            state=IncrementalState(
                items=[
                    IncrementalStateItem(
                        source=Path("src/foo"),
                        destination_relative_to_bundle=Path("foo"),
                        digest="digest",
                        resolved_symlink=None,
                    )
                ],
                codesigned=True,
                codesign_configuration=None,
                codesigned_on_copy=[],
                codesign_identity="same_identity",
                codesign_arguments=["--force"],
                swift_stdlib_paths=[],
                versioned_if_macos=True,
            ),
            codesigned=True,
            codesign_configuration=None,
            codesign_identity="same_identity",
            codesign_arguments=["--force", "--deep"],
            versioned_if_macos=True,
        )
        self.assertFalse(should_assemble_incrementally(spec, incremental_context))
        incremental_context.codesign_arguments = ["--force"]
        self.assertTrue(should_assemble_incrementally(spec, incremental_context))

    def test_not_run_incrementally_when_codesign_configurations_mismatch(self):
        spec = [
            BundleSpecItem(
                src="src/foo",
                dst="foo",
                codesign_on_copy=True,
            )
        ]
        incremental_context = IncrementalContext(
            metadata={Path("src/foo"): "digest"},
            state=IncrementalState(
                items=[
                    IncrementalStateItem(
                        source=Path("src/foo"),
                        destination_relative_to_bundle=Path("foo"),
                        digest="digest",
                        resolved_symlink=None,
                    )
                ],
                codesigned=True,
                # Dry codesigned in old build
                codesign_configuration=CodesignConfiguration.dryRun,
                codesigned_on_copy=[
                    CodesignedOnCopy(
                        path=Path("foo"),
                        entitlements_digest=None,
                        codesign_flags_override=None,
                    )
                ],
                codesign_identity="same_identity",
                codesign_arguments=[],
                versioned_if_macos=True,
                swift_stdlib_paths=[],
            ),
            codesigned=True,
            codesign_configuration=CodesignConfiguration.dryRun,
            codesign_identity="same_identity",
            codesign_arguments=[],
            versioned_if_macos=True,
        )
        # Canary
        self.assertTrue(should_assemble_incrementally(spec, incremental_context))
        # Now we want a regular signing in new build
        incremental_context.codesign_configuration = None
        self.assertFalse(should_assemble_incrementally(spec, incremental_context))

    def test_calculate_incremental_state(self):
        with tempfile.TemporaryDirectory() as project_root, chdir(project_root):
            # project_root
            #           ├── foo
            #           ├── bar
            #           │    ├── baz
            #           │    └── qux -> baz
            #           ├── abc
            #           │    └── def
            #           └── ghi -> abc
            Path("foo").write_text("hello")
            bar_path = Path("bar")
            bar_path.mkdir()
            (bar_path / "baz").write_text("world")
            (bar_path / "qux").symlink_to("baz")
            abc_path = Path("abc")
            abc_path.mkdir()
            (abc_path / "def").write_text("yo")
            Path("ghi").symlink_to("abc")

            action_metadata = {
                Path("foo"): "hash(foo)",
                Path("bar/baz"): "hash(baz)",
                Path("abc/def"): "hash(def)",
            }
            spec = [
                BundleSpecItem(
                    src="foo",
                    dst="foo",
                    codesign_on_copy=False,
                ),
                BundleSpecItem(
                    src="bar",
                    dst="tux",
                    codesign_on_copy=True,
                ),
                BundleSpecItem(
                    src="ghi",
                    dst="ghi",
                    codesign_on_copy=True,
                ),
            ]
            state = calculate_incremental_state(spec, action_metadata)
            self.assertEqual(
                state,
                [
                    IncrementalStateItem(
                        source=Path("foo"),
                        destination_relative_to_bundle=Path("foo"),
                        digest="hash(foo)",
                        resolved_symlink=None,
                    ),
                    IncrementalStateItem(
                        source=Path("bar/baz"),
                        destination_relative_to_bundle=Path("tux/baz"),
                        digest="hash(baz)",
                        resolved_symlink=None,
                    ),
                    IncrementalStateItem(
                        source=Path("bar/qux"),
                        destination_relative_to_bundle=Path("tux/qux"),
                        digest=None,
                        resolved_symlink=Path("baz"),
                    ),
                    IncrementalStateItem(
                        source=Path("ghi/def"),
                        destination_relative_to_bundle=Path("ghi/def"),
                        digest="hash(def)",
                        resolved_symlink=None,
                    ),
                ],
            )

    def test_calculate_incremental_state_with_ds_store(self) -> None:
        with tempfile.TemporaryDirectory() as project_root, chdir(project_root):
            # project_root
            #           ├── foo
            #           └── bar
            #                ├── baz
            #                └── .DS_Store
            Path("foo").write_text("hello")
            bar_path = Path("bar")
            bar_path.mkdir()
            (bar_path / "baz").write_text("world")
            (bar_path / ".DS_Store").touch()

            action_metadata = {
                Path("foo"): "hash(foo)",
                Path("bar/baz"): "hash(baz)",
            }
            spec = [
                BundleSpecItem(
                    src="foo",
                    dst="foo",
                    codesign_on_copy=False,
                ),
                BundleSpecItem(
                    src="bar",
                    dst="bar",
                    codesign_on_copy=True,
                ),
            ]
            state = calculate_incremental_state(spec, action_metadata)
            self.assertEqual(
                state,
                [
                    IncrementalStateItem(
                        source=Path("foo"),
                        destination_relative_to_bundle=Path("foo"),
                        digest="hash(foo)",
                        resolved_symlink=None,
                    ),
                    IncrementalStateItem(
                        source=Path("bar/baz"),
                        destination_relative_to_bundle=Path("bar/baz"),
                        digest="hash(baz)",
                        resolved_symlink=None,
                    ),
                ],
            )
