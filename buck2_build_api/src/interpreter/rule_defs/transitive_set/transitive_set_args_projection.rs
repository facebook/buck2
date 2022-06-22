/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt;
use std::fmt::Display;

use anyhow::Context as _;
use gazebo::any::ProvidesStaticType;
use gazebo::coerce::coerce;
use gazebo::coerce::Coerce;
use gazebo::prelude::*;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::values::display::ContainerDisplayHelper;
use starlark::values::Freeze;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::Value;
use starlark::values::ValueLike;

use crate::artifact_groups::ArtifactGroup;
use crate::artifact_groups::TransitiveSetProjectionKey;
use crate::interpreter::rule_defs::cmd_args::CommandLineArgLike;
use crate::interpreter::rule_defs::cmd_args::CommandLineArtifactVisitor;
use crate::interpreter::rule_defs::cmd_args::CommandLineBuilder;
use crate::interpreter::rule_defs::cmd_args::ValueAsCommandLineLike;
use crate::interpreter::rule_defs::cmd_args::WriteToFileMacroVisitor;
use crate::interpreter::rule_defs::transitive_set::transitive_set_definition_from_value;
use crate::interpreter::rule_defs::transitive_set::traversal::TransitiveSetArgsProjectionTraversal;
use crate::interpreter::rule_defs::transitive_set::TransitiveSet;

#[derive(Debug, Clone, Trace, Coerce, Freeze, ProvidesStaticType)]
#[derive(NoSerialize)] // TODO we should probably have a serialization for transitive set
#[repr(C)]
pub struct TransitiveSetArgsProjectionGen<V> {
    pub(super) transitive_set: V,

    /// The index of the projection. Once transitive sets are defined, their projections never
    /// change, so we can afford to just store the index here.
    pub projection: usize,
}

impl<'v, V: ValueLike<'v>> Display for TransitiveSetArgsProjectionGen<V> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let projection_name = self.projection_name().unwrap_or("<invalid projection>");
        let mut helper = ContainerDisplayHelper::begin(f, "TransitiveSetProjection(", 2)?;
        helper.item(projection_name)?;
        helper.keyed_item("transitive_set", "=", &self.transitive_set)?;
        helper.end(")")
    }
}

impl<'v, V: ValueLike<'v>> TransitiveSetArgsProjectionGen<V> {
    fn projection_name(&self) -> anyhow::Result<&'v str> {
        let set = TransitiveSet::from_value(self.transitive_set.to_value())
            .context("Invalid transitive_set")?;

        let def =
            transitive_set_definition_from_value(set.definition).context("Invalid definition")?;

        Ok(def
            .operations()
            .args_projections
            .get_index(self.projection)
            .context("Invalid projection id")?
            .0
            .as_str())
    }
}

impl<'v, V: ValueLike<'v>> TransitiveSetArgsProjectionGen<V> {
    pub(super) fn iter_values<'a>(
        &'a self,
    ) -> anyhow::Result<Box<dyn Iterator<Item = Value<'v>> + 'a>>
    where
        'v: 'a,
    {
        let set = TransitiveSet::from_value(self.transitive_set.to_value())
            .context("Invalid transitive_set")?;

        let mut iter = set.iter().values().peekable();

        // Defensively, check the projection is valid. We know the set has the same definition
        // throughout so it'll be safe (enough) to unwrap if it is valid on the first one.
        if let Some(v) = iter.peek() {
            v.args_projections
                .get(self.projection)
                .context("Invalid projection")?;
        }

        Ok(box iter.map(move |node| {
            node.args_projections
                .get(self.projection)
                .unwrap()
                .to_value()
        }))
    }
}

starlark_complex_value!(pub TransitiveSetArgsProjection);

impl<'v, V: ValueLike<'v> + 'v> StarlarkValue<'v> for TransitiveSetArgsProjectionGen<V>
where
    Self: ProvidesStaticType,
{
    starlark_type!("transitive_set_args_projection");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(transitive_set_args_projection_methods)
    }
}

impl CommandLineArgLike for FrozenTransitiveSetArgsProjection {
    fn add_to_command_line(&self, builder: &mut dyn CommandLineBuilder) -> anyhow::Result<()> {
        let this: &TransitiveSetArgsProjection = coerce(self);
        this.add_to_command_line(builder)
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        let this: &TransitiveSetArgsProjection = coerce(self);
        this.visit_artifacts(visitor)
    }

    fn contains_arg_attr(&self) -> bool {
        let this: &TransitiveSetArgsProjection = coerce(self);
        this.contains_arg_attr()
    }

    fn visit_write_to_file_macros(
        &self,
        visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        let this: &TransitiveSetArgsProjection = coerce(self);
        this.visit_write_to_file_macros(visitor)
    }
}

impl<'v> CommandLineArgLike for TransitiveSetArgsProjection<'v> {
    fn add_to_command_line(&self, builder: &mut dyn CommandLineBuilder) -> anyhow::Result<()> {
        let set = TransitiveSet::from_value(self.transitive_set.to_value())
            .context("Invalid transitive_set")?;

        for node in set.iter().values() {
            let projection = node
                .args_projections
                .get(self.projection)
                .context("Invalid projection id")?;

            projection
                .as_command_line_err()?
                .add_to_command_line(builder)?;
        }

        Ok(())
    }

    fn visit_artifacts(&self, visitor: &mut dyn CommandLineArtifactVisitor) -> anyhow::Result<()> {
        let set = TransitiveSet::from_value(self.transitive_set.to_value())
            .context("Invalid transitive_set")?;

        visitor.visit_input(
            ArtifactGroup::TransitiveSetProjection(TransitiveSetProjectionKey {
                key: set.key().dupe(),
                projection: self.projection,
            }),
            None,
        );

        Ok(())
    }

    fn contains_arg_attr(&self) -> bool {
        false
    }

    fn visit_write_to_file_macros(
        &self,
        _visitor: &mut dyn WriteToFileMacroVisitor,
    ) -> anyhow::Result<()> {
        Ok(())
    }
}

#[starlark_module]
fn transitive_set_args_projection_methods(builder: &mut MethodsBuilder) {
    fn traverse<'v>(this: Value<'v>, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        Ok(heap.alloc(TransitiveSetArgsProjectionTraversal { inner: this }))
    }
}
