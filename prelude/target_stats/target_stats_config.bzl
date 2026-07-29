# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

load("@prelude//utils:buckconfig.bzl", "read_bool")

# Whether target_stats collection is enabled, from the buckconfig
# `[target_stats] enabled` (root cell). Read at load time because rule
# implementations cannot call read_config; supporting rules gate all of their
# target_stats actions, subtargets, and providers on this constant, so when it
# is False none of that work is defined at all.
TARGET_STATS_ENABLED = read_bool("target_stats", "enabled", default = False, root_cell = True)
