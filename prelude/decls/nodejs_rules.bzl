# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load(":common.bzl", "buck", "prelude_rule")

nodejs_library = prelude_rule(
    name = "nodejs_library",
    docs = """A Node.js library that packages TypeScript/JavaScript source
    files as-is (no compilation). Each entry of `srcs` is staged under
    `dist/<short_path>` inside the package (repeating the same source is
    allowed; distinct sources mapping to the same staged path fail,
    and staged paths must not nest, e.g. `dist/a` and `dist/a/b` fail).
    Each entry of `resources` is staged the same way, for non-code files
    (including generated artifacts and directory artifacts from other
    rules) that ship with the package. `deps` must be `nodejs_library`
    targets; use `resources` for non-code artifacts.
    Staging copies files so the package runs under Node without
    `--preserve-symlinks`. `package_json`, when supplied, is staged at the
    package root as `package.json`. Its `name` is not checked against
    `package_name`; keep the two in sync. Without it, importing by package
    name does not resolve in Node; for packages that must be imported by name,
    supply `package_json` whose `exports` (or `main`) entry points at the
    staged file, e.g. `{"exports": {".": "./dist/<file>"}}`.
    `package_name` (default: target name): a valid npm package name,
    `name` or `@scope/name`, where each segment contains only lowercase
    letters, digits, `-`, `.`, `_`, `~` and does not start with `.` or `_`.
    The default output is the package's own directory
    (`package_dir`). The `[node_modules]` sub-target is the merged
    `node_modules` tree for this package and its transitive `deps`,
    laid out by `package_name`. A duplicate `package_name` in the
    transitive closure with a different package output fails the build.""",
    examples = None,
    further = None,
    attrs = (
        # @unsorted-dict-items
        {
            "srcs": attrs.list(attrs.source(), default = []),
            "resources": attrs.list(attrs.source(), default = []),
            "package_name": attrs.option(attrs.string(), default = None),
            "package_json": attrs.option(attrs.source(allow_directory = False), default = None),
            "deps": attrs.list(attrs.dep(), default = []),
        }
        | buck.labels_arg()
        | buck.contacts_arg()
    ),
)

nodejs_rules = struct(
    nodejs_library = nodejs_library,
)
