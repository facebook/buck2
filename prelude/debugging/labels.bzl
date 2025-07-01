# Copyright (c) Meta Platforms, Inc. and affiliates.
#
# This source code is dual-licensed under either the MIT license found in the
# LICENSE-MIT file in the root directory of this source tree or the Apache
# License, Version 2.0 found in the LICENSE-APACHE file in the root directory
# of this source tree. You may select, at your option, one of the
# above-listed licenses.

### Lookup rules used by BXL script (See fdb.bxl) driven by the labels below:
# 1. If target has no labels we assume a given target is a rule provider and it exposes relevant providers that BXL script depends on for a given language rule

# 2. If target has a label `dbg:info:exec=//another:target` then we assume that `ExecInfo` (see types.bzl) will be obtained via [RunInfo] of another target (//another:target).
# For example:
#    Running "buck run //another:target" (or via using [RunInfo]) should produce `ExecInfo` as its stdout

# 3. If target has a label `dbg:info:ref=//another:target` we assume a presence of //another:target which we can inspect for the presence of relevant providers (see fdb.bxl)

# This label indicates where to locate "[RunInfo]" which would output `ExecInfo` -compatible output
DBG_INFO_EXEC = "dbg:info:exec"

# This label indicates where to locate "rule provider" for a given target name. Rule providers contains language/framework
# specific information that help debugging tools to properly configure a debugger. (Support for any given language/rule needs to be implemented in fdb.bxl)
DBG_INFO_REF = "dbg:info:ref"

DBG_INFO_DISABLE_INCOMPATIBLE_SANITIZERS = "dbg:info:disable-sanitizers"

def dbg_info_exec(target_label) -> list[str]:
    return ["{}={}".format(DBG_INFO_EXEC, target_label)]

def dbg_info_ref(target_label) -> list[str]:
    return ["{}={}".format(DBG_INFO_REF, target_label)]

def get_info_ref(labels: list[str]) -> [str, None]:
    for label in labels:
        result = get_value_by_mark(DBG_INFO_REF, label)
        if result:
            return result
    return None

def get_label_or_mark(label: str) -> str:
    for mark in [DBG_INFO_EXEC, DBG_INFO_REF]:
        if label.startswith(mark):
            return mark
    return label

def get_value_by_mark(mark: str, label: str) -> [str, None]:
    if label.startswith(mark + "="):
        return label.removeprefix(mark + "=")
    return None
