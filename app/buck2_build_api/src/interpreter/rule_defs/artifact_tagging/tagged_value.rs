/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use allocative::Allocative;
use derive_more::Display;
use starlark::any::ProvidesStaticType;
use starlark::coerce::Coerce;
use starlark::environment::GlobalsBuilder;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::values::Freeze;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLifetimeless;
use starlark::values::ValueLike;
use starlark::values::starlark_value;
use starlark::values::starlark_value_as_type::StarlarkValueAsType;

use super::ArtifactTag;
use super::TaggedVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;

#[derive(
    Debug,
    Clone,
    Trace,
    Coerce,
    Freeze,
    Display,
    ProvidesStaticType,
    Allocative
)]
#[derive(NoSerialize)] // TODO make artifacts serializable
#[repr(C)]
#[display("TaggedValue({}, tagged {})", inner, tag)]
pub struct StarlarkTaggedValueGen<V: ValueLifetimeless> {
    inner: V,
    tag: ArtifactTag,
    inputs_only: bool,
}

impl<'v> StarlarkTaggedValue<'v> {
    pub fn new(inner: Value<'v>, tag: ArtifactTag) -> Self {
        Self {
            inner,
            tag,
            inputs_only: false,
        }
    }

    pub fn inputs_only(inner: Value<'v>, tag: ArtifactTag) -> Self {
        Self {
            inner,
            tag,
            inputs_only: true,
        }
    }
}

starlark_complex_value!(pub StarlarkTaggedValue);

#[starlark_value(type = "TaggedValue")]
impl<'v, V: ValueLike<'v>> StarlarkValue<'v> for StarlarkTaggedValueGen<V>
where
    Self: ProvidesStaticType<'v>,
{
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(tagged_value_methods)
    }
}

/// Opaque type returned by [`ArtifactTag.tag_artifacts()`](../ArtifactTag#artifacttagtag_artifacts)
/// or [`ArtifactTag.tag_inputs()`](../ArtifactTag#artifacttagtag_inputs) for non-command-line like values.
///
/// For complete documentation, see [`ctx.actions.artifact_tag()`](../AnalysisActions#analysisactionsartifact_tag).
#[starlark_module]
fn tagged_value_methods(_: &mut MethodsBuilder) {}

impl<V: ValueLifetimeless> StarlarkTaggedValueGen<V> {
    pub fn value(&self) -> &V {
        &self.inner
    }

    pub fn wrap_visitor<'a, 'b, 'v>(
        &'a self,
        visitor: &'b mut dyn CommandLineArtifactVisitor<'v>,
    ) -> TaggedVisitor<'a, 'b, 'v> {
        TaggedVisitor::wrap(&self.tag, self.inputs_only, visitor)
    }
}

#[starlark_module]
pub(crate) fn register_tagged_value(globals: &mut GlobalsBuilder) {
    const TaggedValue: StarlarkValueAsType<StarlarkTaggedValue> = StarlarkValueAsType::new();
}
