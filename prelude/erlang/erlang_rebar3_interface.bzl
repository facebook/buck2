# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(":erlang_dependencies.bzl", "flatten_dependencies")
load(":erlang_info.bzl", "ErlangAppInfo", "ErlangRebar3TestsInfo")
load(":erlang_utils.bzl", "to_term_args")

def check_and_sort_dependencies(in_deps: ["dependency"]):
    app_deps = []
    test_deps = []
    for dep in in_deps:
        if ErlangRebar3TestsInfo in dep:
            test_deps.append(dep)
        elif ErlangAppInfo in dep:
            app_deps.append(dep)
        else:
            fail((
                "unsupported dependency through target `%s`: " +
                "the target needs to define an Erlang application " +
                "or migration test target"
            ) % (str(dep.label),))

    return (app_deps, test_deps)

def build_run_info(
        ctx: "context",
        build_env_file: "artifact",
        outputs: ["artifact"]) -> "provider":
    # next, build the script that copies the file in the right location
    script_content = cmd_args([])
    script_content.add(['echo "writing dependency information to $1"'])
    script_content.add(cmd_args(
        ["cp $(buck2 root --kind=project)/", build_env_file, " $1"],
        delimiter = "",
    ))
    script_content.add("")
    script_file = ctx.actions.write("dump_dependency.sh", script_content)

    # run the script
    shell_cmd = cmd_args(["/bin/bash", script_file])
    shell_cmd.hidden(build_env_file)
    shell_cmd.hidden(outputs)

    return RunInfo(shell_cmd)

def erlang_rebar3_interface_impl(ctx: "context") -> ["provider"]:
    # collect all dependencies
    direct_app_deps, test_deps = check_and_sort_dependencies(ctx.attrs.deps)

    app_deps = flatten_dependencies(ctx, direct_app_deps)

    outputs = []

    build_info = {}
    for dep in test_deps:
        name = dep[ErlangRebar3TestsInfo].app_name
        test_dir = dep[ErlangRebar3TestsInfo].test_dir
        build_info[name] = {
            "test": cmd_args(test_dir, format = "\"{}\""),
        }
        outputs.append(test_dir)

    for name, dep in app_deps.items():
        if not name in build_info:
            build_info[name] = {}
        if ErlangAppInfo in dep:
            dep_info = dep[ErlangAppInfo]
            if dep_info.virtual:
                # skip virtual apps
                continue
            build_info[name]["ebin"] = cmd_args(dep_info.app_folder, format = "\"{}/ebin\"")
            build_info[name]["include"] = cmd_args(dep_info.app_folder, format = "\"{}/include\"")
            build_info[name]["priv"] = cmd_args(dep_info.app_folder, format = "\"{}/priv\"")
            outputs += dep[DefaultInfo].default_outputs
        else:
            # we are not including include_only deps
            pass

    # first, build the .term file containing the build info
    build_env_file = ctx.actions.write(
        "build_environment.term",
        to_term_args(build_info),
    )

    run_info = build_run_info(ctx, build_env_file, outputs)

    return [
        DefaultInfo(default_output = build_env_file),
        run_info,
    ]
