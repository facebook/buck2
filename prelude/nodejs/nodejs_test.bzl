# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//:prelude_rule.bzl", "prelude_rule")
load("@prelude//decls:common.bzl", "buck")
load("@prelude//decls:re_test_common.bzl", "re_test_common")
load("@prelude//decls:test_common.bzl", "test_common")
load("@prelude//decls:toolchains_common.bzl", "toolchains_common")
load("@prelude//test:inject_test_run_info.bzl", "inject_test_run_info")
load("@prelude//tests:re_utils.bzl", "get_re_executors_from_props")
load(":nodejs_binary.bzl", "build_nodejs_command", "nodejs_command", "stage_nodejs_entries")

# TPX reads per-test results from `TEST_RESULTS_OUTPUT_FILE` without copying the run's output into each.
_PER_TEST_RESULT_LABELS = ["tpx:supports-test-result-output-spec", "tpx:bundled-tests-have-per-test-details"]

# `node --test` with no arguments runs `**/{test,test/**/*,test-*,*[._-]test}.{js,mjs,cjs}` outside
# `node_modules`, and the TypeScript extensions too unless type stripping is off.
_NODE_TEST_JS_EXTENSIONS = ["js", "mjs", "cjs"]
_NODE_TEST_TS_EXTENSIONS = ["ts", "mts", "cts"]

def _is_node_test_file(short_path: str, strip_types: bool) -> bool:
    parts = short_path.split("/")
    dirs = parts[:-1]
    if "node_modules" in dirs:
        return False
    stem, dot, ext = parts[-1].rpartition(".")
    if not dot or not (ext in _NODE_TEST_JS_EXTENSIONS or (strip_types and ext in _NODE_TEST_TS_EXTENSIONS)):
        return False
    return "test" in dirs or stem == "test" or stem.startswith("test-") or stem.endswith((".test", "-test", "_test"))

def _node_test_files(ctx: AnalysisContext) -> list[Artifact]:
    if ctx.attrs.library != None:
        return []
    strip_types = "--no-strip-types" not in ctx.attrs.node_args and "--no-experimental-strip-types" not in ctx.attrs.node_args
    test_files = [src for src in ctx.attrs.srcs if _is_node_test_file(src.short_path, strip_types)]
    if not test_files:
        fail(
            "{}: no `srcs` entry is a node:test test file; node:test runs `**/{{test,test/**/*,test-*,*[._-]test}}.{{js,mjs,cjs{}}}` outside `node_modules`".format(
                ctx.label,
                ",ts,mts,cts" if strip_types else "",
            )
        )
    return test_files

def _framework_command(ctx: AnalysisContext) -> struct:
    if ctx.attrs.main != None:
        fail('{}: `main` requires framework = "none"; list the test files in `srcs`'.format(ctx.label))
    if ctx.attrs.args:
        fail(
            '{}: `args` cannot be combined with framework = "{}", which takes no script arguments; use framework = "none" to pass `args` to `main`'.format(
                ctx.label, ctx.attrs.framework
            )
        )
    for reserved in ("LIST_COMMAND", "TEST_COMMAND"):
        if reserved in ctx.attrs.env:
            fail('{}: `env` cannot set `{}` with framework = "{}", which uses it to list and run tests'.format(ctx.label, reserved, ctx.attrs.framework))
    staged = stage_nodejs_entries(ctx, _node_test_files(ctx), mains_attr = "`srcs`", staged_attrs = "`srcs`")
    args = cmd_args(ctx.attrs._test_adapter, "--framework", ctx.attrs.framework, hidden = staged.hidden)
    for arg in ctx.attrs.node_args:
        args.add("--node-arg", arg)
    for entry in staged.entries:
        args.add("--test-file", entry.name, entry.path)
    return struct(merged = staged.runtime, output = nodejs_command(ctx, args, staged.env), env = staged.env)

def nodejs_test_impl(ctx: AnalysisContext) -> list[Provider]:
    if ctx.attrs.framework == "none":
        command = build_nodejs_command(ctx)
        test_type = "custom"
        env = command.env
        labels = ctx.attrs.labels
    else:
        command = _framework_command(ctx)
        test_type = "js_test"
        env = command.env | {
            "LIST_COMMAND": cmd_args(command.output.cmd, "--list"),
            "TEST_COMMAND": cmd_args(command.output.cmd, "--write-test-results"),
        }
        labels = ctx.attrs.labels + [label for label in _PER_TEST_RESULT_LABELS if label not in ctx.attrs.labels]

    re_executors = get_re_executors_from_props(ctx)
    use_project_relative_paths = "buck2_run_from_project_root" in ctx.attrs.labels or re_executors.use_project_relative_paths

    return inject_test_run_info(
        ctx,
        ExternalRunnerTestInfo(
            type = test_type,
            command = [command.output.cmd],
            env = env,
            labels = labels,
            contacts = ctx.attrs.contacts,
            default_executor = re_executors.default_executor,
            executor_overrides = re_executors.executor_overrides,
            run_from_project_root = True,
            use_project_relative_paths = use_project_relative_paths or None,
            supports_test_execution_caching = ctx.attrs.supports_test_execution_caching,
        ),
    ) + [
        DefaultInfo(default_output = command.merged, other_outputs = list(command.output.output.default_outputs) + list(command.output.output.other_outputs)),
    ]

nodejs_test = prelude_rule(
    name = "nodejs_test",
    impl = nodejs_test_impl,
    docs = """A Node.js test, run with the toolchain's Node runtime and no
    custom loaders. `framework` decides what runs and what is reported.

    `"node"` (the default) runs each test file with Node's built-in test
    runner, `node:test` (Node 20 or later), in a process of its own. The
    test files are the `srcs` that `node --test` with no arguments would
    run: short paths matching `**/*.test.<ext>`, `**/*-test.<ext>`,
    `**/*_test.<ext>`, `**/test-*.<ext>`, `**/test.<ext>` or
    `**/test/**/*.<ext>` outside `node_modules`, where `<ext>` is `js`,
    `mjs` or `cjs`, or `ts`, `mts` or `cts` unless `node_args` turns off
    type stripping (`--no-strip-types` or `--no-experimental-strip-types`). TypeScript test files need a Node
    that strips types by default (22.18 or later), or
    `--experimental-strip-types` in `node_args` (22.6 or later). The
    other `srcs` are helpers, declared as inputs but not run. It fails if no `srcs` entry is a test file. With
    `library`, the test file is the `entry_point`. `main` and `args` cannot
    be set, and `node_args` are passed to each test file's Node process.
    When several test files run at once, each line they print is prefixed
    with `[<test file>] `.

    Each test file gets a result named by its short path (by the
    `entry_point` for `library`), and each test that finishes gets one
    named `<test file> > <describe> > ... > <test>`. A `describe` block
    gets one only when it is skipped or fails on its own, and a test that
    fails only because a subtest failed gets none. Test names should
    be unique among siblings, which otherwise share one result name. A test
    file fails if any of its tests fails, if its process exits non-zero, or
    if its results are incomplete: the process exited before node:test
    reported the end of the run (as it does for a file that defines no
    tests), or node:test's events could not be read in full. The failure
    names any tests that started but never finished.

    The test is an external runner test of type `js_test`. `LIST_COMMAND`
    writes one JSON line `{"testcase": <test file>}` per test file to the
    file named by an appended `--output <file>`. `TEST_COMMAND` runs the
    test files and tests named by appended `--filter <name>` arguments
    (every test file without one) and, when `TEST_RESULTS_OUTPUT_FILE` is
    set, writes the results there in the test result output spec. A filter
    names a test file, a `describe` block (every test in it) or a test, and
    fails if it matches no test. Filtering below a test file needs Node 22
    or later and cannot select a subtest of a `test()`. Run on its own, for
    example by `buck2 run`, the test runs every test file (or those
    `--filter` arguments name), writes no results, and exits non-zero if
    anything failed.

    `"none"` runs `main`, or the `library`'s `entry_point`, with `args` and
    `node_args`, and the exit code is the only result. `srcs` are helpers.

    When `deps` or `library` is set, builds a directory holding a merged
    `node_modules` from transitive deps, and runs from it so Node's own
    resolution finds the deps. With `deps`, it stages `srcs`, and `main`
    for `"none"`, beside `node_modules` (each at its short path; distinct files staging
    to the same path fail) and runs the staged files; with
    `library`, it runs the `entry_point` from inside the library's package
    under `node_modules`. The default output is that directory. Without
    them, the test files run at their original paths, files they import
    via relative paths must be listed in `srcs` to be declared as build
    inputs, and there is no default output.

    Deps must be loadable as regular `node_modules` packages: each
    package's `package.json` governs whether it is CommonJS or an ES
    module, and TypeScript must be compiled to JavaScript before it is
    packaged. The merged tree is also exposed via `NODE_PATH` at runtime,
    appended to any `NODE_PATH` set in `env`, for CommonJS resolution.
    Transitive deps' package directories must be unique, as for
    `nodejs_library`. It runs via a `command_alias` wrapper script for the
    target platform, which sets `env` (including `NODE_PATH` from `deps`)
    on Unix and Windows. `node_modules` appends prebuilt trees to
    `NODE_PATH`. `library`, `entry_point` and `node_modules` follow the same
    combination rules and `.d.ts` stripping as `nodejs_binary`.""",
    examples = None,
    further = None,
    attrs = (
        # @unsorted-dict-items
        {
            "main": attrs.option(attrs.source(), default = None),
            "srcs": attrs.list(attrs.source(), default = []),
            "framework": attrs.enum(["node", "none"], default = "node"),
            "args": attrs.list(attrs.arg(), default = []),
            "node_args": attrs.list(attrs.string(), default = []),
            "env": attrs.dict(attrs.string(), attrs.arg(), default = {}),
            "deps": attrs.list(attrs.dep(), default = []),
            "library": attrs.option(attrs.dep(), default = None),
            "entry_point": attrs.option(attrs.string(), default = None),
            "node_modules": attrs.list(attrs.dep(), default = []),
            "supports_test_execution_caching": attrs.bool(default = False),
            "_nodejs_toolchain": toolchains_common.nodejs(),
            "_target_os_type": buck.target_os_type_arg(),
            "_test_adapter": attrs.default_only(attrs.source(default = "prelude//nodejs/tools:nodejs_test_adapter.cjs")),
        }
        | buck.labels_arg()
        | buck.contacts_arg()
        | buck.inject_test_env_arg()
        | buck.test_rule_timeout_ms()
        | test_common.attributes()
        | re_test_common.test_args()
    ),
)
