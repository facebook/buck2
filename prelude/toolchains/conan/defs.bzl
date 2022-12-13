# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""Conan C/C++ Package Manager Toolchain.

Provides a toolchain and rules to use the [Conan package manager][conan] to
manage and install third-party C/C++ dependencies.
"""

ConanInitInfo = provider(fields = ["user_home"])
ConanLockInfo = provider(fields = ["lockfile"])
ConanPackageInfo = provider(fields = ["reference", "cache_out"])
ConanToolchainInfo = provider(fields = ["conan"])

def _conan_generate_impl(ctx: "context") -> ["provider"]:
    conan_toolchain = ctx.attrs._conan_toolchain[ConanToolchainInfo]
    conan_init = ctx.attrs._conan_init[ConanInitInfo]
    conan_generate = ctx.attrs._conan_generate[RunInfo]

    install_folder = ctx.actions.declare_output("install-folder")
    output_folder = ctx.actions.declare_output("output-folder")
    user_home = ctx.actions.declare_output("user-home")
    manifests = ctx.actions.declare_output("manifests")
    install_info = ctx.actions.declare_output("install-info.json")
    trace_log = ctx.actions.declare_output("trace.log")
    targets_out = ctx.actions.declare_output(ctx.label.name + ".bzl")

    cmd = cmd_args([conan_generate])
    cmd.add(["--conan", conan_toolchain.conan])
    cmd.add(["--conan-init", conan_init.user_home])
    cmd.add(["--buckler", ctx.attrs._buckler])
    cmd.add(["--install-folder", install_folder.as_output()])
    cmd.add(["--output-folder", output_folder.as_output()])
    cmd.add(["--user-home", user_home.as_output()])
    cmd.add(["--manifests", manifests.as_output()])
    cmd.add(["--install-info", install_info.as_output()])
    cmd.add(["--trace-file", trace_log.as_output()])
    cmd.add(["--conanfile", ctx.attrs.conanfile])
    cmd.add(["--lockfile", ctx.attrs.lockfile])
    cmd.add(["--targets-out", targets_out.as_output()])
    ctx.actions.run(cmd, category = "conan_build")

    return [
        # TODO[AH] ConanGenerateInfo with the generated targets file
        DefaultInfo(
            default_outputs = [targets_out],
            other_outputs = [
                install_folder,
                output_folder,
                user_home,
                manifests,
                install_info,
                trace_log,
            ],
        ),
    ]

conan_generate = rule(
    impl = _conan_generate_impl,
    attrs = {
        "conanfile": attrs.source(doc = "The conanfile defining the project dependencies."),
        "lockfile": attrs.source(doc = "The Conan lockfile pinning the package versions."),
        "_conan_toolchain": attrs.default_only(attrs.toolchain_dep(default = "toolchains//:conan", providers = [ConanToolchainInfo])),
        "_conan_init": attrs.dep(providers = [ConanInitInfo], default = "toolchains//:conan-init"),
        "_buckler": attrs.source(default = "prelude//toolchains/conan:buckler"),
        "_conan_generate": attrs.dep(providers = [RunInfo], default = "prelude//toolchains/conan:conan_generate"),
    },
)

def _conan_init_impl(ctx: "context") -> ["provider"]:
    conan_toolchain = ctx.attrs._conan_toolchain[ConanToolchainInfo]
    conan_init = ctx.attrs._conan_init[RunInfo]

    user_home = ctx.actions.declare_output("user-home")
    trace_log = ctx.actions.declare_output("trace.log")

    cmd = cmd_args([conan_init])
    cmd.add(["--conan", conan_toolchain.conan])
    cmd.add(["--user-home", user_home.as_output()])
    cmd.add(["--trace-file", trace_log.as_output()])
    ctx.actions.run(cmd, category = "conan_init")

    return [
        ConanInitInfo(
            user_home = user_home,
        ),
        DefaultInfo(default_outputs = [
            user_home,
            trace_log,
        ]),
    ]

conan_init = rule(
    impl = _conan_init_impl,
    attrs = {
        "_conan_toolchain": attrs.default_only(attrs.toolchain_dep(default = "toolchains//:conan", providers = [ConanToolchainInfo])),
        "_conan_init": attrs.dep(providers = [RunInfo], default = "prelude//toolchains/conan:conan_init"),
    },
)

def _conan_lock_impl(ctx: "context") -> ["provider"]:
    conan_toolchain = ctx.attrs._conan_toolchain[ConanToolchainInfo]
    conan_init = ctx.attrs._conan_init[ConanInitInfo]
    conan_lock = ctx.attrs._conan_lock[RunInfo]

    lockfile_out = ctx.actions.declare_output("conan.lock")
    user_home = ctx.actions.declare_output("user-home")
    trace_log = ctx.actions.declare_output("trace.log")

    cmd = cmd_args([conan_lock])
    cmd.add(["--conan", conan_toolchain.conan])
    cmd.add(["--conan-init", conan_init.user_home])
    cmd.add(["--user-home", user_home.as_output()])
    cmd.add(["--trace-file", trace_log.as_output()])
    cmd.add(["--conanfile", ctx.attrs.conanfile])
    cmd.add(["--lockfile-out", lockfile_out.as_output()])
    if ctx.attrs.lockfile:
        cmd.add(["--lockfile", ctx.attrs.lockfile])
    ctx.actions.run(cmd, category = "conan_lock")

    return [
        ConanLockInfo(
            lockfile = lockfile_out,
        ),
        DefaultInfo(
            default_outputs = [lockfile_out],
            other_outputs = [user_home, trace_log],
        ),
    ]

conan_lock = rule(
    impl = _conan_lock_impl,
    attrs = {
        "conanfile": attrs.source(doc = "The conanfile defining the project dependencies."),
        "lockfile": attrs.option(attrs.source(doc = "A pre-existing lockfile to base the dependency resolution on."), default = None),
        "_conan_toolchain": attrs.default_only(attrs.toolchain_dep(default = "toolchains//:conan", providers = [ConanToolchainInfo])),
        "_conan_init": attrs.dep(providers = [ConanInitInfo], default = "toolchains//:conan-init"),
        "_conan_lock": attrs.dep(providers = [RunInfo], default = "prelude//toolchains/conan:conan_lock"),
    },
)

def _conan_package_impl(ctx: "context") -> ["provider"]:
    conan_toolchain = ctx.attrs._conan_toolchain[ConanToolchainInfo]
    conan_init = ctx.attrs._conan_init[ConanInitInfo]
    conan_package = ctx.attrs._conan_package[RunInfo]

    install_folder = ctx.actions.declare_output("install-folder")
    output_folder = ctx.actions.declare_output("output-folder")
    user_home = ctx.actions.declare_output("user-home")
    manifests = ctx.actions.declare_output("manifests")
    install_info = ctx.actions.declare_output("install-info.json")
    trace_log = ctx.actions.declare_output("trace.log")
    cache_out = ctx.actions.declare_output("cache-out")

    cmd = cmd_args([conan_package])
    cmd.add(["--conan", conan_toolchain.conan])
    cmd.add(["--conan-init", conan_init.user_home])
    cmd.add(["--buckler", ctx.attrs._buckler])
    cmd.add(["--lockfile", ctx.attrs.lockfile])
    cmd.add(["--reference", ctx.attrs.reference])
    cmd.add([cmd_args(ctx.attrs.options, prepend = "--option")])
    cmd.add(["--install-folder", install_folder.as_output()])
    cmd.add(["--output-folder", output_folder.as_output()])
    cmd.add(["--user-home", user_home.as_output()])
    cmd.add(["--manifests", manifests.as_output()])
    cmd.add(["--install-info", install_info.as_output()])
    cmd.add(["--trace-file", trace_log.as_output()])
    cmd.add(["--cache-out", cache_out.as_output()])
    for dep in ctx.attrs.deps:
        info = dep[ConanPackageInfo]
        cmd.add(["--dep-reference", info.reference, "--dep-cache-out", info.cache_out])
    ctx.actions.run(cmd, category = "conan_build")

    return [
        ConanPackageInfo(
            reference = ctx.attrs.reference,
            cache_out = cache_out,
        ),
        DefaultInfo(default_outputs = [
            install_folder,
            output_folder,
            user_home,
            manifests,
            install_info,
            trace_log,
            cache_out,
        ]),
    ]

conan_package = rule(
    impl = _conan_package_impl,
    attrs = {
        "lockfile": attrs.source(doc = "The Conan lockfile defining the package and its dependencies."),
        "reference": attrs.string(doc = "The Conan package reference <name>/<version>#<revision>."),
        "options": attrs.list(attrs.string(doc = "Conan build options.")),
        "deps": attrs.list(attrs.dep(providers = [ConanPackageInfo], doc = "Conan Package dependencies.")),
        "_conan_toolchain": attrs.default_only(attrs.toolchain_dep(default = "toolchains//:conan", providers = [ConanToolchainInfo])),
        "_conan_init": attrs.dep(providers = [ConanInitInfo], default = "toolchains//:conan-init"),
        "_conan_package": attrs.dep(providers = [RunInfo], default = "prelude//toolchains/conan:conan_package"),
        "_buckler": attrs.source(default = "prelude//toolchains/conan:buckler"),
    },
)

def _conan_update_impl(ctx: "context") -> ["provider"]:
    conan_update = ctx.attrs._conan_update[RunInfo]

    cmd = cmd_args([conan_update])
    cmd.add(["--lockfile", ctx.attrs.lockfile])
    cmd.add(["--lock-targets", ctx.attrs.lock_generate])
    cmd.add(["--conan-targets", ctx.attrs.conan_generate])
    cmd.add(["--conanfile", ctx.attrs.conanfile])
    cmd.add(["--lockfile-out", ctx.attrs.lockfile_name])
    cmd.add(["--targets-out", ctx.attrs.targets_name])

    return [
        DefaultInfo(default_outputs = []),
        RunInfo(args = [cmd]),
    ]

conan_update = rule(
    impl = _conan_update_impl,
    attrs = {
        "lockfile": attrs.source(doc = "The generated Conan lockfile."),
        "lock_generate": attrs.source(doc = "The targets generated from the Conan lockfile."),
        "conan_generate": attrs.source(doc = "The targets generated by Buckler."),
        "conanfile": attrs.source(doc = "The Conanfile."),
        "lockfile_name": attrs.string(doc = "Generate a lockfile with this name next to the Conanfile."),
        "targets_name": attrs.string(doc = "Generate a TARGETS file with this name next to the Conanfile."),
        "_conan_update": attrs.dep(providers = [RunInfo], default = "prelude//toolchains/conan:conan_update"),
    },
    doc = "Defines a runnable target that will update the Conan lockfile and import targets.",
)

def _relative_label(lbl: "label", relto: "label") -> "string":
    """Return a string representation of `lbl` relative to `relto`.

    E.g. `root//some/package:here` is `:here` relative to `root//some/package:there`.
    """
    if lbl.cell != relto.cell:
        tpl = "{cell}//{package}:{name}"
    elif lbl.package != relto.package:
        tpl = "//{package}:{name}"
    else:
        tpl = ":{name}"
    if lbl.sub_target:
        tpl += "[{sub_target}]"
    return tpl.format(
        cell = lbl.cell,
        package = lbl.package,
        name = lbl.name,
        sub_target = lbl.sub_target,
    )

def _lock_generate_impl(ctx: "context") -> ["provider"]:
    lock_generate = ctx.attrs._lock_generate[RunInfo]

    targets_out = ctx.actions.declare_output(ctx.label.name + ".bzl")

    cmd = cmd_args([lock_generate])
    cmd.add(["--lockfile", ctx.attrs.lockfile])
    cmd.add(["--lockfile-label", _relative_label(ctx.attrs.lockfile.owner, ctx.label)])
    cmd.add(["--targets-out", targets_out.as_output()])
    ctx.actions.run(cmd, category = "conan_generate")

    return [
        DefaultInfo(
            default_outputs = [targets_out],
        ),
    ]

lock_generate = rule(
    impl = _lock_generate_impl,
    attrs = {
        "lockfile": attrs.source(doc = "The Conan lockfile defining the package and its dependencies."),
        "_lock_generate": attrs.dep(providers = [RunInfo], default = "prelude//toolchains/conan:lock_generate"),
    },
)

def _system_conan_toolchain_impl(ctx: "context") -> ["provider"]:
    return [
        DefaultInfo(),
        ConanToolchainInfo(
            conan = RunInfo(args = [ctx.attrs.conan_path]),
        ),
    ]

system_conan_toolchain = rule(
    impl = _system_conan_toolchain_impl,
    attrs = {
        "conan_path": attrs.string(doc = "Path to the Conan executable."),
    },
    is_toolchain_rule = True,
    doc = "Uses a globally installed Conan executable.",
)
