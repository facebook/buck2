# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is licensed under both the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree and the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree.

# We only want to document the rules in the prelude, so have a module that
# only reexports those.

load("@prelude//:rules.bzl", _rules = "rules")

# We don't use namespace(name=r) because that would pass the literal key 'name'.
# Instead, we use namespace(**{name: r}) to dynamically use the value of the variable `name` as the key.
load_symbols({name: namespace(**{name: r}) for name, r in _rules.items()})
