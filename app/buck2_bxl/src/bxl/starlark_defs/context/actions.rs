/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

//! Starlark Actions API for bxl functions
//!

use allocative::Allocative;
use buck2_build_api::analysis::registry::AnalysisRegistry;
use buck2_build_api::bxl::execution_platform::EXECUTION_PLATFORM;
use buck2_build_api::deferred::base_deferred_key::BaseDeferredKey;
use buck2_build_api::interpreter::rule_defs::context::AnalysisActions;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::starlark_type;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueTyped;
use starlark::StarlarkDocs;
use thiserror::Error;

use crate::bxl::starlark_defs::context::BxlContext;

#[derive(Debug, Error)]
enum BxlActionsError {
    #[error(
        "An action registry was already requested via `actions_factory`. Only one action registry is allowed"
    )]
    RegistryAlreadyCreated,
}

pub(crate) fn validate_action_instantiation<'v>(this: &BxlContext<'v>) -> anyhow::Result<()> {
    let mut registry = this.state.state.borrow_mut();

    if (*registry).is_some() {
        return Err(anyhow::anyhow!(BxlActionsError::RegistryAlreadyCreated));
    } else {
        let analysis_registry = AnalysisRegistry::new_from_owner(
            BaseDeferredKey::BxlLabel(this.current_bxl.dupe()),
            EXECUTION_PLATFORM.dupe(),
        );

        *registry = Some(analysis_registry);
    }

    Ok(())
}

#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    StarlarkDocs,
    Allocative
)]
#[starlark_docs(directory = "bxl")]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
pub(crate) struct BxlActions<'v> {
    actions: ValueTyped<'v, AnalysisActions<'v>>,
}

impl<'v> StarlarkValue<'v> for BxlActions<'v> {
    starlark_type!("bxl_actions");
}

impl<'v> AllocValue<'v> for BxlActions<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> StarlarkTypeRepr for &'v BxlActions<'v> {
    fn starlark_type_repr() -> String {
        BxlActions::get_type_value_static().as_str().to_owned()
    }
}

impl<'v> UnpackValue<'v> for &'v BxlActions<'v> {
    fn unpack_value(x: Value<'v>) -> Option<&'v BxlActions<'v>> {
        x.downcast_ref()
    }
}
