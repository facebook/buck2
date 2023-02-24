/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::sync::Arc;

use allocative::Allocative;
use buck2_build_api::actions::RegisteredAction;
use buck2_build_api::deferred::base_deferred_key::BaseDeferredKey;
use buck2_interpreter::types::target_label::StarlarkConfiguredTargetLabel;
use derive_more::Display;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::starlark_type;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::UnpackValue;
use starlark::values::ValueLike;
use starlark::StarlarkDocs;

#[derive(Debug, Display, ProvidesStaticType, Allocative, StarlarkDocs)]
#[derive(NoSerialize)]
#[display(fmt = "{}", "self.0")]
#[starlark_docs(directory = "bxl")]
pub struct StarlarkAction(pub Arc<RegisteredAction>);

starlark_simple_value!(StarlarkAction);

impl<'v> StarlarkValue<'v> for StarlarkAction {
    starlark_type!("action");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(action_methods)
    }
}

impl<'a> UnpackValue<'a> for StarlarkAction {
    fn expected() -> String {
        "action".to_owned()
    }

    fn unpack_value(value: starlark::values::Value<'a>) -> Option<Self> {
        value
            .downcast_ref::<Self>()
            .map(|value| Self(value.0.dupe()))
    }
}

/// Methods for an action.
#[starlark_module]
fn action_methods(builder: &mut MethodsBuilder) {
    /// Gets the owning configured target label for an action.
    ///
    /// Sample usage:
    /// ```text
    /// def _impl_action(ctx):
    ///     action = ctx.audit().output("buck-out/path/to/__target__/artifact", "your_target_platform")
    ///     ctx.output.print(action.owner())
    /// ```
    fn owner<'v>(this: StarlarkAction) -> anyhow::Result<StarlarkConfiguredTargetLabel> {
        match this.0.owner() {
            BaseDeferredKey::TargetLabel(label) => {
                Ok(StarlarkConfiguredTargetLabel::new(label.dupe()))
            }
            _ => Err(anyhow::anyhow!("BXL and anon targets not supported.")),
        }
    }
}
