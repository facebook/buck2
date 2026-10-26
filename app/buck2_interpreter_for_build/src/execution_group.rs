/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use buck2_interpreter::execution_group::EXEC_GROUP_TYPE;
use buck2_interpreter::execution_group::FrozenStarlarkExecutionGroup;
use buck2_interpreter::execution_group::StarlarkExecutionGroup;
use starlark::environment::GlobalsBuilder;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;

use crate::interpreter::build_context::BuildContext;

#[starlark_module]
fn register_execution_group_globals(r: &mut GlobalsBuilder) {
    const ExecGroup: StarlarkValueAsType<StarlarkExecutionGroup> = EXEC_GROUP_TYPE;

    /// Create a new execution group.
    ///
    /// The value returned must be immediately bound to a global, which names the group:
    ///
    /// ```python
    /// Link = exec_group()
    ///
    /// my_rule = rule(
    ///     impl = _impl,
    ///     attrs = {},
    ///     exec_groups = [Link],
    /// )
    /// ```
    fn exec_group<'v>(
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<StarlarkExecutionGroup> {
        let cell_path = BuildContext::from_context(eval)?
            .starlark_path()
            .path()
            .into_owned();
        Ok(StarlarkExecutionGroup::new_unbound(cell_path))
    }

    /// The execution group of every exec dep, toolchain dep and action that does not specify one.
    /// Its execution platform is the target's execution platform.
    const DefaultExecGroup: FrozenStarlarkExecutionGroup = FrozenStarlarkExecutionGroup::DEFAULT;
}

pub(crate) fn register_execution_group(globals: &mut GlobalsBuilder) {
    register_execution_group_globals(globals);
}
