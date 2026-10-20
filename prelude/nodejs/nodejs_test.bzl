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
load(":nodejs_providers.bzl", "get_nodejs_tree_info")
load(":nodejs_toolchain.bzl", "NodejsToolchainInfo")

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

def _jest_install(ctx: AnalysisContext) -> Artifact:
    if ctx.attrs.test_runner != None:
        return get_nodejs_tree_info(ctx.attrs.test_runner, consumer_label = ctx.label, attr = "test_runner").node_modules
    jest = ctx.attrs._nodejs_toolchain[NodejsToolchainInfo].jest
    if jest == None:
        fail('{}: framework = "jest" requires `jest` on the nodejs toolchain, or `test_runner`'.format(ctx.label))
    return jest

def _framework_command(ctx: AnalysisContext) -> struct:
    jest = None
    if ctx.attrs.framework == "jest":
        if ctx.attrs.library != None:
            fail(
                '{}: `library` cannot be combined with framework = "jest", which ignores test files under `node_modules`, where `library` stages `entry_point`; list the test files in `srcs` and the library in `deps`'.format(
                    ctx.label
                )
            )
        jest = _jest_install(ctx)
    if ctx.attrs.main != None:
        fail('{}: `main` requires framework = "none"; list the test files in `srcs`'.format(ctx.label))
    if ctx.attrs.args and jest == None:
        fail(
            '{}: `args` cannot be combined with framework = "{}", which takes no script arguments; use framework = "none" to pass `args` to `main`'.format(
                ctx.label, ctx.attrs.framework
            )
        )
    for reserved in ("LIST_COMMAND", "TEST_COMMAND"):
        if reserved in ctx.attrs.env:
            fail('{}: `env` cannot set `{}` with framework = "{}", which uses it to list and run tests'.format(ctx.label, reserved, ctx.attrs.framework))
    if jest != None:
        if ctx.attrs.config != None:
            for src in ctx.attrs.srcs:
                if src.short_path == ctx.attrs.config.short_path and src != ctx.attrs.config:
                    fail("{}: `config` '{}' and `srcs` entry '{}' both stage to '{}'".format(ctx.label, ctx.attrs.config, src, src.short_path))
        mains = ctx.attrs.srcs
        srcs = ctx.attrs.srcs + ([ctx.attrs.config] if ctx.attrs.config != None else [])
    else:
        mains = _node_test_files(ctx)
        srcs = None

    # Jest looks for tests and modules under a root directory, so its files are always staged.
    staged = stage_nodejs_entries(ctx, mains, mains_attr = "`srcs`", staged_attrs = "`srcs`", srcs = srcs, stage = jest != None)
    args = cmd_args(ctx.attrs._test_adapter, "--framework", ctx.attrs.framework, hidden = staged.hidden)
    for arg in ctx.attrs.node_args:
        args.add("--node-arg", arg)
    if jest != None:
        args.add("--jest", jest, "--root", staged.runtime)
        if ctx.attrs.config != None:
            args.add("--config", cmd_args(staged.runtime, format = "{}/" + ctx.attrs.config.short_path))
        for arg in ctx.attrs.args:
            args.add("--arg", arg)
    for entry in staged.entries:
        args.add("--test-file", entry.name, entry.path)
    return struct(merged = staged.runtime, output = nodejs_command(ctx, args, staged.env), env = staged.env)

def nodejs_test_impl(ctx: AnalysisContext) -> list[Provider]:
    if ctx.attrs.framework != "jest":
        for attr in ("config", "test_runner"):
            if getattr(ctx.attrs, attr) != None:
                fail('{}: `{}` requires framework = "jest"'.format(ctx.label, attr))
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
    runner, `node:test` (Node 20 or later), in a process of its own. Its
    test files are the `srcs` that `node --test` with no arguments would
    run: short paths matching `**/*.test.<ext>`, `**/*-test.<ext>`,
    `**/*_test.<ext>`, `**/test-*.<ext>`, `**/test.<ext>` or
    `**/test/**/*.<ext>` outside `node_modules`, where `<ext>` is `js`,
    `mjs` or `cjs`, or `ts`, `mts` or `cts` unless `node_args` turns off
    type stripping (`--no-strip-types` or `--no-experimental-strip-types`).
    TypeScript needs Node 22.18+, or 22.6+ with `--experimental-strip-types`.
    With `library`, the test file is the `entry_point`. `args` cannot be
    set. Test files running at once prefix each line they print with
    `[<test file>] `.

    `"jest"` runs the test files with Jest from `test_runner` (a
    `nodejs_node_modules` target), or the nodejs toolchain's `jest`. Its
    test files are the `srcs` that the Jest config selects, as
    `jest --listTests` reports them; `library` cannot be set. `config`, a
    Jest config file, is staged at its short path, and Jest's `rootDir`
    defaults to the config file's directory; without it, `rootDir` is the
    staged directory. `args` are passed to Jest, except the watch modes,
    the flags the adapter sets (`--ci`, `--watchman`, `--config`,
    `--json`, `--outputFile`, `--listTests`, `--runTestsByPath`,
    `--testNamePattern`) and the flags that change which test files run
    (`--onlyChanged`, `--onlyFailures`, `--changedSince`,
    `--changedFilesWithAncestor`, `--lastCommit`, `--findRelatedTests`,
    `--shard`), which fail the test.

    The other `srcs` are helpers, not run, and `main` cannot be set.
    Without `library`, the test fails if no `srcs` entry is a test file: at
    analysis with `"node"`, when listing or running with `"jest"`.
    `node_args` go to the Node processes running the test files. Each test
    file gets a result named by its short path (by the `entry_point` for
    `library`), and each test that finishes gets one named
    `<test file> > <describe> > ... > <test>`. With `"node"`, a
    `describe` block gets one only when it is skipped or fails on its own,
    and a test that fails only because a subtest failed gets none.
    Same-named sibling tests share a result name. A test file
    fails if any of its tests fails. With `"jest"`, it also fails if it
    fails to load or Jest reports no result for it. With `"node"`, it also
    fails if its process exits non-zero or before node:test reports the
    end of the run, or its events cannot be read in full, naming tests
    that never finished. With `"jest"`, an unsuccessful Jest run with no
    failed test file, as for a missed coverage threshold, is a run
    failure.

    The test is an external runner test of type `js_test`. `LIST_COMMAND`
    writes one JSON line `{"testcase": <test file>}` per test file to the
    file named by an appended `--output <file>`. `TEST_COMMAND` runs
    the test files and tests named by appended `--filter <name>` arguments
    (every test file without one) and, when `TEST_RESULTS_OUTPUT_FILE` is
    set, writes results there in the test result output spec. A filter
    names a test file, a `describe` block (every test in it) or a test, and
    fails if it matches no test. With `"jest"`, a filter below a test file
    ignores case. With `"node"`, filtering below a test file
    needs Node 22 or later and cannot select a subtest of a `test()`. Run
    on its own, as by `buck2 run`, the test writes no results and exits non-zero
    on failure.

    `"none"` runs `main`, or the `library`'s `entry_point`, with `args` and
    `node_args`; its exit code is the only result. `srcs` are helpers.

    When `deps` or `library` is set, or with `"jest"`, the test runs from
    a directory, its default output. With `deps` or `library`, it holds a
    merged `node_modules` from transitive deps. Without `library`, `srcs`
    and `main` are staged there at their short paths, which must not
    collide; with `library`, the `entry_point` runs in the library's
    package under `node_modules`. Otherwise, the test files run in place,
    and files they import relatively must be in `srcs` to be build inputs.

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
            "framework": attrs.enum(["node", "jest", "none"], default = "node"),
            "config": attrs.option(attrs.source(allow_directory = False), default = None),
            "test_runner": attrs.option(attrs.dep(), default = None),
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
