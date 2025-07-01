# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

"""
Modifier aliases that can be used on the CLI, ex. after `--modifier=` or in `?`.

These aliases are ones we share between internal and OSS usages.
All constraints used in these aliases must also be available in OSS.
"""

# It's represented as a struct where the attribute name is the alias and the string
# for the attribute is the fully qualified target. Defining aliases in a struct
# helps enforce that the alias names do not contain any bad character we cannot use on CLI.
#
# We define aliases for modifiers here rather than reusing `alias` section of buckconfig for
# several reasons.
# 1. `alias` buckconconfig is not well-designed. It only supports aliases in a cell and not
# global aliases and users can override aliases in modefiles.
# 2. Modifier aliases can point to conditional modifiers, which `alias` buckconfig does not
# suppport.
# 3. It's unlikely a user has to ever define an alias twice in both the `alias` buckconfig
# and in modifier aliases because a modifier alias is a constraint value/config setting
# and those don't typically get built on CLI.
OSS_ALIASES = struct()
