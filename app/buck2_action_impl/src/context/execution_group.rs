/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_core::execution_types::execution_group::ExecutionGroup;
use buck2_interpreter::execution_group::ExecutionGroupArg;
use starlark::values::none::NoneOr;

/// The execution group of an action from its `exec_group` argument: the default group when
/// it is `None`.
pub(crate) fn action_execution_group(execution_group: NoneOr<ExecutionGroupArg>) -> ExecutionGroup {
    match execution_group.into_option() {
        None => ExecutionGroup::Default,
        Some(ExecutionGroupArg { group }) => group,
    }
}
