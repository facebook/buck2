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
use buck2_build_api::actions::query::ActionQueryNode;
use buck2_build_api::actions::RegisteredAction;
use buck2_core::base_deferred_key::BaseDeferredKey;
use buck2_interpreter::types::target_label::StarlarkConfiguredTargetLabel;
use buck2_query::query::environment::QueryTarget;
use derive_more::Display;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::starlark_value;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::UnpackValue;
use starlark::values::ValueLike;
use starlark::values::ValueTyped;
use starlark::StarlarkDocs;

use crate::bxl::starlark_defs::analysis_result::StarlarkAnalysisResult;

#[derive(Debug, Display, ProvidesStaticType, Allocative, StarlarkDocs)]
#[derive(NoSerialize)]
#[display(fmt = "{}", "self.0")]
#[starlark_docs(directory = "bxl")]
pub(crate) struct StarlarkAction(pub(crate) Arc<RegisteredAction>);

starlark_simple_value!(StarlarkAction);

#[starlark_value(type = "action")]
impl<'v> StarlarkValue<'v> for StarlarkAction {
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

#[derive(Debug, Display, ProvidesStaticType, Allocative, StarlarkDocs)]
#[derive(NoSerialize)]
#[display(fmt = "{}", "self.0.key()")]
#[starlark_docs(directory = "bxl")]
pub(crate) struct StarlarkActionQueryNode(pub(crate) ActionQueryNode);

starlark_simple_value!(StarlarkActionQueryNode);

#[starlark_value(type = "action_query_node")]
impl<'v> StarlarkValue<'v> for StarlarkActionQueryNode {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(action_query_node_value_methods)
    }
}

impl<'a> UnpackValue<'a> for StarlarkActionQueryNode {
    fn expected() -> String {
        "action query node".to_owned()
    }

    fn unpack_value(value: starlark::values::Value<'a>) -> Option<Self> {
        value
            .downcast_ref::<Self>()
            .map(|value| Self(value.0.dupe()))
    }
}

/// Methods for action query node.
#[starlark_module]
fn action_query_node_value_methods(builder: &mut MethodsBuilder) {
    /// Gets optional action from the action query target node.
    fn action<'v>(
        this: &StarlarkActionQueryNode,
        heap: &'v Heap,
    ) -> anyhow::Result<Option<ValueTyped<'v, StarlarkAction>>> {
        Ok(this
            .0
            .action()
            .map(|a| heap.alloc_typed(StarlarkAction(a.clone()))))
    }

    /// Gets optional analysis from the action query target node.
    fn analysis<'v>(
        this: &StarlarkActionQueryNode,
        heap: &'v Heap,
    ) -> anyhow::Result<Option<ValueTyped<'v, StarlarkAnalysisResult>>> {
        Ok(this.0.analysis_opt().map(|a| {
            heap.alloc_typed(StarlarkAnalysisResult::new(
                a.analysis_result().clone(),
                a.target().as_ref().clone(),
            ))
        }))
    }

    /// Gets the kind of action query node, either analysis or action kind.
    #[starlark(attribute)]
    fn rule_type(this: &StarlarkActionQueryNode) -> anyhow::Result<String> {
        Ok(this.0.rule_type().to_string())
    }
}
