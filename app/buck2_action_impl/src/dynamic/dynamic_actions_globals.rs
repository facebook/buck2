/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::cell::OnceCell;

use starlark::environment::GlobalsBuilder;
use starlark::starlark_module;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;
use starlark::values::typing::StarlarkCallableChecked;

use crate::dynamic::dynamic_actions::StarlarkDynamicActions;
use crate::dynamic::dynamic_actions_callable::DynamicActionsCallable;
use crate::dynamic::dynamic_actions_callable::DynamicActionsCallbackParamSpec;
use crate::dynamic::dynamic_actions_callable::DynamicActionsCallbackReturnType;
use crate::dynamic::dynamic_actions_callable::FrozenStarlarkDynamicActionsCallable;

#[starlark_module]
pub(crate) fn register_dynamic_actions(globals: &mut GlobalsBuilder) {
    /// Create new dynamic action callable. Returned object will be callable,
    /// and the result of calling it can be passed to `ctx.actions.dynamic_output_new`.
    fn dynamic_actions<'v>(
        #[starlark(require = named)] r#impl: StarlarkCallableChecked<
            'v,
            DynamicActionsCallbackParamSpec,
            DynamicActionsCallbackReturnType,
        >,
    ) -> anyhow::Result<DynamicActionsCallable<'v>> {
        Ok(DynamicActionsCallable {
            implementation: r#impl.to_unchecked(),
            name: OnceCell::new(),
        })
    }

    const DynamicActions: StarlarkValueAsType<StarlarkDynamicActions> = StarlarkValueAsType::new();
    const DynamicActionsCallable: StarlarkValueAsType<FrozenStarlarkDynamicActionsCallable> =
        StarlarkValueAsType::new();
}
