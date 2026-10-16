# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

# The declaration of a rule that the prelude exposes via `native` and documents.
#
# Conventions:
#  - A rule is declared in the `.bzl` file that implements it, next to its
#    implementation function.
#  - Attribute groups shared between the rules of one language live in that
#    language's `attrs.bzl`; groups shared across languages live in `decls/`.
#    Keep `attrs.bzl` files free of implementation code so that implementation
#    files can load them without cycles.
#  - Every language directory has a `rules.bzl` exporting `rules`, the
#    declarations it contributes, keyed by rule name. `prelude//:rules.bzl`
#    collects those and turns them into `rule` objects, applying the prelude-wide
#    policies (fat platform compatibility and the like) so that individual
#    declarations don't have to.
prelude_rule = record(
    name = field(str),
    docs = field([str, None], None),
    examples = field([str, None], None),
    further = field([str, None], None),
    attrs = field(dict[str, Attr]),
    impl = field([typing.Callable, None], None),
    uses_plugins = field([list[plugins.PluginKind], None], None),
    supports_incoming_transition = field([bool, None], None),
    is_toolchain_rule = field(bool, False),
    is_configuration_rule = field(bool, False),
    # TODO(mgd): should be `transition | None`, but `transition` does not work as type.
    cfg = field(typing.Any | None, None),
)
