/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use allocative::Allocative;
use buck2_core::target::label::TargetLabel;
use derivative::Derivative;
use derive_more::Display;
use starlark::any::ProvidesStaticType;
use starlark::values::starlark_value;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::StarlarkDocs;

use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::value_as_starlark_target_label::ValueAsStarlarkTargetLabel;

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
#[allocative(skip)]
pub(crate) struct StarlarkAQueryCtx<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    ctx: &'v BxlContext<'v>,
    #[derivative(Debug = "ignore")]
    target_platform: Option<TargetLabel>,
}

#[starlark_value(type = "aqueryctx", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkAQueryCtx<'v> {}

impl<'v> AllocValue<'v> for StarlarkAQueryCtx<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> StarlarkAQueryCtx<'v> {
    pub(crate) fn new(
        ctx: &'v BxlContext<'v>,
        global_target_platform: Value<'v>,
        default_target_platform: &Option<TargetLabel>,
    ) -> anyhow::Result<StarlarkAQueryCtx<'v>> {
        let target_platform = global_target_platform.parse_target_platforms(
            &ctx.data.target_alias_resolver,
            &ctx.data.cell_resolver,
            ctx.data.cell_name,
            default_target_platform,
        )?;

        Ok(Self {
            ctx,
            target_platform,
        })
    }
}
