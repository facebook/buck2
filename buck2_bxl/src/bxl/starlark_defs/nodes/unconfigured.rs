/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use buck2_build_api::{
    interpreter::rule_defs::target_label::StarlarkTargetLabel, nodes::unconfigured::TargetNode,
};
use derive_more::Display;
use gazebo::{any::ProvidesStaticType, prelude::*};
use starlark::{
    environment::{Methods, MethodsBuilder, MethodsStatic},
    starlark_module, starlark_simple_value, starlark_type,
    values::{Heap, NoSerialize, StarlarkValue, UnpackValue, Value, ValueLike},
};

use crate::bxl::starlark_defs::nodes::unconfigured::attribute::StarlarkTargetNodeCoercedAttributes;

pub mod attribute;

#[derive(Debug, Display, ProvidesStaticType)]
#[derive(NoSerialize)] // TODO probably should be serializable the same as how queries serialize
#[display(fmt = "{:?}", self)]
pub struct StarlarkTargetNode(pub TargetNode);

starlark_simple_value!(StarlarkTargetNode);

impl<'v> StarlarkValue<'v> for StarlarkTargetNode {
    starlark_type!("target_node");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(target_node_value_methods)
    }
}

impl<'a> UnpackValue<'a> for StarlarkTargetNode {
    fn expected() -> String {
        "target node".to_owned()
    }

    fn unpack_value(value: starlark::values::Value<'a>) -> Option<Self> {
        value
            .downcast_ref::<Self>()
            .map(|value| Self(value.0.dupe()))
    }
}

#[starlark_module]
fn target_node_value_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn attributes<'v>(this: StarlarkTargetNode, heap: &Heap) -> anyhow::Result<Value<'v>> {
        Ok(heap.alloc(StarlarkTargetNodeCoercedAttributes {
            inner: heap.alloc(this),
        }))
    }

    #[starlark(attribute)]
    fn label(this: &StarlarkTargetNode) -> anyhow::Result<StarlarkTargetLabel> {
        Ok(this.0.label().dupe().into())
    }
}
