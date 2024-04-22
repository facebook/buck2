# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

MockingbirdLibraryInfoTSet = transitive_set()

MockingbirdTargetType = enum("library", "test")

MockingbirdLibraryInfo = provider(
    fields = {
        # The name of the target.
        "name": provider_field(str),
        # Contains a tset with this target's MockingbirdLibraryRecord as the value
        # and all of its dependency's MockingbirdLibraryRecord in the children.
        "tset": provider_field(MockingbirdLibraryInfoTSet),
    },
)

MockingbirdLibraryRecord = record(
    # The names of this target's dependencies.
    dep_names = field(list[str]),
    # The names of this target's exported dependencies.
    exported_dep_names = field(list[str]),
    # The name of the target.
    name = str,
    # Swift sources in this target.
    srcs = field(list[Artifact]),
    # Whether this is a library or a test.
    type = field(MockingbirdTargetType),
    # Symlinked directory containing the source files.
    src_dir = field(Artifact),
)

MockingbirdSourcesInfo = provider(
    fields = {
        # Source files containing the auto generated mocks produced by mockingbird-cli.
        "srcs": provider_field(list[Artifact]),
    },
)
