# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

load("@prelude//linking:shared_libraries.bzl", "SharedLibrariesTSet")
load("@prelude//utils:arglike.bzl", "ArgLike")

# DistInfo is a provider that indicates what other targets/artifacts might be
# necessary to ship alongside the current target as part of a package or other
# distribution mechanism, such as shared libraries, default configuration,
# graphical/media assets, etc.
DistInfo = provider(fields = {
    # The collection of other artifacts that must be available when an
    # executable is executed, such as resources.
    #
    # These files are typically a subset of `DefaultInfo.other_outputs` of the
    # executable. `other_outputs` may contain other auxiliary helpful but
    # nonessential runtime files, such as external debuginfo which one would
    # need in order to run the executable in a debugger.
    #
    # When an executable is the end goal of a build (i.e. `buck2 build :main`)
    # and `--materializations=none` has not been passed, then all of
    # `other_outputs` is what gets materialized. For convenience to developers,
    # we choose to make this include external debuginfo, rather than requiring
    # them to build something like `buck2 build :main :main[debuginfo]` in order
    # to have an executable that is debuggable locally.
    #
    # In contrast, `nondebug_runtime_files` are the things required always in
    # any context where an executable runs. For example when this executable is
    # the exe in a genrule -- there would be no point materializing its external
    # debuginfo in that situation because no debugger is involved.
    #
    # There are yet other contexts where not even `nondebug_runtime_files` would
    # need to be materialized. For example a rule that depends on an executable
    # in order to compute a checksum or signature for it. As the executable does
    # not run during that action, runtime files would not be needed. As such,
    # bundling `nondebug_runtime_files` into an artifact group with the
    # executable artifact itself, using `artifact.with_associated_artifacts`, is
    # not a universally suitable alternative to nondebug runtime files.
    #
    # Unlike `ResourceInfo` this provider does not attempt to identify the label
    # from which each of these artifacts originates. This is just the projected
    # set of all the files for materialization purposes.
    "nondebug_runtime_files": provider_field(list[ArgLike]),

    # Transitive shared library dependencies.
    "shared_libs": provider_field(SharedLibrariesTSet | None, default = None),
})
