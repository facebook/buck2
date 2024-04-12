# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

"""
Handles labels used to provide compilation database information for filegroup() and genrule() targets.

Our language services need to know how to compile files owned solely by filegroup() or genrule() targets like:
* Regular generated sources, that then end up being compiled by regular cxx_ or apple_ targets.
* Manually declared mixin files, that are always compiled by multiple other targets spread across the codebase.
* Files built by external build systems wrapped in genrules(), where compile_commands.json is produced by yet another genrule().

Prior approach for the former two cases was to run rdeps() queries to find a compilable target that would have a compile_commands.json entry for the file.
It suffered from reliability and performance issues, as the universe for rdeps() queries had to be quite broad and with no guarantee that there isn't even a single broken target within it.
And for external build system wrappers where there is no compilable target, we could define a rule that would effectively wrap two genrules and expose one of them as [compilation-database] subtarget,
but that wouldn't solve the problem with mixins which is still relevant with external build systems and would put us in the same suboptimal spot in terms of performance and reliability.

As the IDE needs to operate in O(changes) instead of O(repo), and open files even if some other corner of the repo is broken.
We need to make things both reliable and performant in an ever-growing codebase with a CI that explicitly cannot guarantee that the entire repo is green, and where rdeps() queries are thus flaky and slow.

And as the IDE needs to react to any local changes and act consistently with local checkout, we cannot simply use a remote cache for rdeps() queries that are slow and flaky.

So the solution is instead to localize the required information within the target, and directly point to the build system rules that provide compile_commands.json for the target.
"""

def compilation_database_rules(source_mapping: dict[str, list[str]] | list[str]) -> list[str]:
    """
    Takes a mapping from sources to the rules to be used to build compilation databases for those sources.

    Tooling like IDEs needs to obtain compile commands for source files that are exported by filegroup() to be built as part of another target, or are built with an external build system wrapped in a genrule().
    Labels provide a convenient way to link the non-compileable target with a rule that produces a compilation database for its sources:
    ```
    load("@prelude//cxx:compilation_database_labels.bzl", "compilation_database_rules")

    # The shorthand way for most cases:
    export_file(
        name = "gadget_to_be_compiled_as_part_of_another_target.cpp",
        labels = compilation_database_rules([
            "//path/to/some/dependent:target",
            "//path/to/another/dependent:target",
        ])
    )

    # A per-source mapping for cases when the generated files from one genrule() are compiled in different targets and never together:
    genrule(
        name = "multiple_gadgets_for_different_purposes",
        labels = compilation_database_rules({
            "server_gen.cpp": ["//path/to/dependent/module:server"],
            "client_gen.cpp": ["//path/to/dependent/module:client"],
        })
    )
    ```
    The tooling can use a BXL script to check the target kind and extract the compilation database rule from its labels. And then iterate over the resulting compilation database and resolve the symlinks in 'file' entries in order to find the matching entry for the original source.

    :param dict[str,str]|list[str] source_mapping: A mapping with source file name regex as key and target as value. The target has to be either a target with [compilation-database] subtarget, or a genrule that produces compile_commands.json (for wrapping external build systems).
    """
    if not isinstance(source_mapping, dict):
        source_mapping = {".*": source_mapping}
    return ["compilation_database_rules=" + json.encode(source_mapping)]

def get_compilation_database_rules(labels = list[str]) -> dict[str, list[str]] | None:
    """
    Retrieves and decodes compilation database targets from target labels, if any.
    """
    for label in labels:
        value = label.removeprefix("compilation_database_rules=")
        if value != label:
            return json.decode(value)
    return None
