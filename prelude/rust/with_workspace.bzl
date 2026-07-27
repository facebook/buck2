# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

package_key = "rust.workspaces"
exclude_patterns_package_key = "rust.workspace_exclude_patterns"
patterns_package_key = "rust.workspace_patterns"

def with_rust_workspace(labels = None, patterns = None, exclude_patterns = None):
    if isinstance(labels, str):
        labels = [labels]
    if isinstance(patterns, str):
        patterns = [patterns]
    if isinstance(exclude_patterns, str):
        exclude_patterns = [exclude_patterns]

    exclude_patterns = exclude_patterns or []

    if bool(labels) == bool(patterns):
        fail("rust_with_workspace requires exactly one of `labels` or `patterns`")
    if exclude_patterns and not patterns:
        fail("rust_with_workspace `exclude_patterns` requires `patterns`")

    parent_labels = read_parent_package_value(package_key) or []
    parent_exclude_patterns = read_parent_package_value(exclude_patterns_package_key) or []
    parent_patterns = read_parent_package_value(patterns_package_key) or []

    if labels:
        if parent_patterns or parent_exclude_patterns:
            fail("rust_with_workspace cannot mix `labels` and `patterns` through PACKAGE inheritance")
        write_package_value(package_key, parent_labels + labels, overwrite = True)
    else:
        if parent_labels:
            fail("rust_with_workspace cannot mix `labels` and `patterns` through PACKAGE inheritance")
        write_package_value(exclude_patterns_package_key, parent_exclude_patterns + exclude_patterns, overwrite = True)
        write_package_value(patterns_package_key, parent_patterns + patterns, overwrite = True)
