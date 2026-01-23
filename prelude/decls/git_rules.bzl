# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":common.bzl", "buck", "prelude_rule")

git_fetch = prelude_rule(
    name = "git_fetch",
    docs = """
        Checkout a commit from a git repository.
    """,
    examples = """
        ```
        git_fetch(
            name = "serde.git",
            repo = "https://github.com/serde-rs/serde",
            rev = "fccb9499bccbaca0b7eef91a3a82dfcb31e0b149",
        )
        ```
    """,
    further = None,
    attrs = (
        # @unsorted-dict-items
        {
            "allow_cache_upload": attrs.bool(doc = """
                Whether the results of the fetch can be written to the action cache and CAS.
            """, default = True),
            "object_format": attrs.option(
                attrs.enum(["sha1", "sha256"]),
                default = None,
                doc = """
                The object format to use for the underlying Git repository.
                Must be one of `sha1` or `sha256`.
                """,
            ),
            "repo": attrs.string(doc = """
                Url suitable as a git remote.
            """),
            "rev": attrs.string(doc = """
                Commit hash. 40 hex digits for sha1, 64 hex digits for sha256.
            """),
            "update_submodules": attrs.bool(
                default = False,
                doc = """
                Whether to initialize and update Git submodules after there worktree checkout.
                """
            ),
            "sub_targets": attrs.list(
                attrs.string(),
                default = [],
                doc = """
                A list of paths within the remote repo to be made accessible as sub-targets.
                For example if we have a git_fetch with `name = "serde.git"` and
                `sub_targets = ["serde_derive"]`, then other targets would be able to refer
                to the serde_derive subdirectory of the repo as `":serde.git[serde_derive]"`.
            """,
            ),
            "default_host_platform": attrs.option(attrs.configuration_label(), default = None),
            "_git_fetch_tool": attrs.default_only(attrs.exec_dep(providers = [RunInfo], default = "prelude//git/tools:git_fetch")),
        } |
        buck.licenses_arg() |
        buck.labels_arg() |
        buck.contacts_arg()
    ),
)

git_rules = struct(
    git_fetch = git_fetch,
)
