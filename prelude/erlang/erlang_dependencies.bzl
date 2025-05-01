# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load(
    ":erlang_info.bzl",
    "ErlangAppIncludeInfo",
    "ErlangAppInfo",
    "ErlangTestInfo",
)

ErlAppDependencies = dict[str, Dependency]

def check_dependencies(in_deps: list[Dependency], allowlist: list):
    """enforce all dependencies implement one of the expected providers"""
    for dep in in_deps:
        passed = False
        for dep_type in allowlist:
            if dep_type in dep:
                passed = True
                break
        if not passed:
            _bad_dependency_error(dep)

def flatten_dependencies(_ctx: AnalysisContext, deps: list[Dependency]) -> ErlAppDependencies:
    """ collect transitive dependencies

    Flatten all transitive dependencies and merge together with the direct
    ones. This is done at every step (with each `erlang_application_impl ` call),
    so we only need to look one hop away.
    """
    dependencies = {}
    for dep in deps:
        if ErlangAppInfo in dep:
            # handle transitive deps
            for trans_dep_val in dep[ErlangAppInfo].dependencies.values():
                dependencies = _safe_add_dependency(dependencies, trans_dep_val)
        elif ErlangTestInfo in dep:
            for trans_dep_val in dep[ErlangTestInfo].dependencies.values():
                dependencies = _safe_add_dependency(dependencies, trans_dep_val)
        dependencies = _safe_add_dependency(dependencies, dep)

    return dependencies

def _safe_add_dependency(dependencies: ErlAppDependencies, dep: Dependency) -> ErlAppDependencies:
    """Adds ErlangAppInfo and ErlangAppIncludeInfo dependencies

    ErlangAppInfo (full) application dependencies overwrite include_only dependencies,
    while include_only dependencies are only added if no other dependency is already
    present.

    ErlangAppInfo applications fail, if there is already another full application with the
    same name added.
    """
    if ErlangAppInfo in dep:
        name = dep[ErlangAppInfo].name
        if name in dependencies and ErlangAppInfo in dependencies[name] and dep.label != dependencies[name].label:
            fail(("duplicated application `%s` in dependency tree:\n" +
                  "    %s\n" +
                  "    %s") % (name, str(dep.label), str(dependencies[name].label)))
        else:
            dependencies[name] = dep
    elif ErlangTestInfo in dep:
        dependencies[dep[ErlangTestInfo].name] = dep
    elif dep[ErlangAppIncludeInfo].name not in dependencies:
        dependencies[dep[ErlangAppIncludeInfo].name] = dep
    return dependencies

def _bad_dependency_error(dep: Dependency):
    fail((
        "unsupported dependency through target `%s`: " +
        "the target needs to define an Erlang application"
    ) % (str(dep.label),))
