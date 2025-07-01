# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

def pick(override, underlying):
    return cmd_args(override) if override != None else underlying

def pick_bin(override, underlying):
    return override[RunInfo] if override != None else underlying

def pick_dep(override, underlying):
    return pick_raw(override, underlying)

def pick_raw(override, underlying):
    return override if override != None else underlying

def pick_and_add(override, additional, underlying):
    flags = [pick(override, underlying)]
    if additional:
        flags.append(additional)
    return cmd_args(flags)
