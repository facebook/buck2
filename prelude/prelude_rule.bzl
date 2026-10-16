# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# The declaration of a rule that the prelude exposes via `native` and documents.
#
# The actual `rule()` object is created from this record in `prelude//:rules.bzl`,
# which also applies the prelude-wide policies (fat platform compatibility and
# the like), so that individual declarations don't have to.
prelude_rule = record(
    name = field(str),
    docs = field([str, None], None),
    examples = field([str, None], None),
    further = field([str, None], None),
    attrs = field(dict[str, Attr]),
    impl = field([typing.Callable, None], None),
    uses_plugins = field([list[plugins.PluginKind], None], None),
    supports_incoming_transition = field([bool, None], None),
    is_toolchain_rule = field([bool, None], None),
    # TODO(mgd): should be `transition | None`, but `transition` does not work as type.
    cfg = field(typing.Any | None, None),
)
