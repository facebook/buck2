# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:prelude_rule.bzl", "prelude_rule")
load("@prelude//decls:common.bzl", "buck")
load("@prelude//decls:toolchains_common.bzl", "toolchains_common")
load("@prelude//test:inject_test_run_info.bzl", "inject_test_run_info")
load(":nodejs_binary.bzl", "build_nodejs_command")

def nodejs_test_impl(ctx: AnalysisContext) -> list[Provider]:
    command = build_nodejs_command(ctx)
    return inject_test_run_info(
        ctx,
        ExternalRunnerTestInfo(
            type = "custom",
            command = [command.output.cmd],
            env = command.env,
            labels = ctx.attrs.labels,
            contacts = ctx.attrs.contacts,
        ),
    ) + [
        DefaultInfo(default_output = command.merged, other_outputs = list(command.output.output.default_outputs) + list(command.output.output.other_outputs)),
    ]

nodejs_test = prelude_rule(
    name = "nodejs_test",
    impl = nodejs_test_impl,
    docs = """A Node.js test target, executed with the toolchain's Node
    runtime and no custom loaders; the exit code is pass/fail. When
    `deps` is non-empty, builds a merged `node_modules` from transitive
    deps, stages `main` and `srcs` beside it (each at its short path;
    distinct files staging to the same path fail), and runs the staged
    `main` so Node's own resolution finds the deps. The default output
    is that directory. With no `deps`, `main` runs at its original path,
    files it imports via relative paths must be listed in `srcs` to be
    declared as build inputs, and there is no default output. Deps must
    be loadable as regular `node_modules` packages: each package's
    `package.json` governs whether it is CommonJS or an ES module, and
    TypeScript must be compiled to JavaScript before it is packaged.
    The merged tree is also exposed via `NODE_PATH` at runtime, appended
    to any `NODE_PATH` set in `env`, for CommonJS resolution. Transitive
    deps' package directories must be unique, as for `nodejs_library`.
    It runs via a `command_alias` wrapper script for the target platform,
    which sets `env` (including `NODE_PATH` from `deps`) on Unix and
    Windows. `library`/`entry_point` run a file from inside a
    `nodejs_library`, and `node_modules` appends prebuilt trees to
    `NODE_PATH`, with the same combination rules and `.d.ts` stripping as
    `nodejs_binary`.""",
    examples = None,
    further = None,
    attrs = (
        # @unsorted-dict-items
        {
            "main": attrs.option(attrs.source(), default = None),
            "srcs": attrs.list(attrs.source(), default = []),
            "args": attrs.list(attrs.arg(), default = []),
            "node_args": attrs.list(attrs.string(), default = []),
            "env": attrs.dict(attrs.string(), attrs.arg(), default = {}),
            "deps": attrs.list(attrs.dep(), default = []),
            "library": attrs.option(attrs.dep(), default = None),
            "entry_point": attrs.option(attrs.string(), default = None),
            "node_modules": attrs.list(attrs.dep(), default = []),
            "_nodejs_toolchain": toolchains_common.nodejs(),
            "_target_os_type": buck.target_os_type_arg(),
        }
        | buck.labels_arg()
        | buck.contacts_arg()
        | buck.inject_test_env_arg()
    ),
)
