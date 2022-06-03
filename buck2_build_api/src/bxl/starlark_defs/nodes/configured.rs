/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::{borrow::Cow, path::Path};

use anyhow::Context;
use buck2_common::dice::{cells::HasCellResolver, data::HasIoProvider};
use buck2_core::{
    cells::paths::CellPath,
    fs::{paths::AbsPath, project::ProjectRelativePath},
    provider::ConfiguredProvidersLabel,
};
use derive_more::Display;
use gazebo::{any::ProvidesStaticType, prelude::*};
use starlark::{
    collections::SmallMap,
    environment::{Methods, MethodsBuilder, MethodsStatic},
    values::{
        structs::Struct, AllocValue, Heap, NoSerialize, StarlarkValue, UnpackValue, Value,
        ValueLike,
    },
};

use crate::{
    actions::artifact::{Artifact, SourceArtifact},
    attrs::{attr_type::attr_literal::ConfiguredAttrTraversal, configured_attr::ConfiguredAttr},
    bxl::starlark_defs::context::BxlContext,
    deferred::AnyValue,
    interpreter::rule_defs::{
        artifact::StarlarkArtifact, target_label::StarlarkConfiguredTargetLabel,
    },
    nodes::configured::ConfiguredTargetNode,
    path::BuckPath,
};

#[derive(Debug, Display, ProvidesStaticType)]
#[derive(NoSerialize)] // TODO probably should be serializable the same as how queries serialize
#[display(fmt = "{:?}", self)]
pub struct StarlarkConfiguredTargetNode(pub ConfiguredTargetNode);

starlark_simple_value!(StarlarkConfiguredTargetNode);

impl<'v> StarlarkValue<'v> for StarlarkConfiguredTargetNode {
    starlark_type!("target_node");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(configured_target_node_value_methods)
    }
}

impl<'a> UnpackValue<'a> for StarlarkConfiguredTargetNode {
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
fn configured_target_node_value_methods(builder: &mut MethodsBuilder) {
    #[starlark(attribute)]
    fn label(this: &StarlarkConfiguredTargetNode) -> anyhow::Result<StarlarkConfiguredTargetLabel> {
        Ok(StarlarkConfiguredTargetLabel::new(this.0.name().dupe()))
    }

    fn attributes<'v>(
        this: &StarlarkConfiguredTargetNode,
        heap: &Heap,
    ) -> anyhow::Result<Value<'v>> {
        let mut attrs = SmallMap::with_capacity(this.0.attrs().size_hint().0);
        for (name, attr) in this.0.attrs() {
            attrs.insert(
                heap.alloc_str(name),
                heap.alloc(StarlarkConfiguredValue(attr)),
            );
        }

        Ok(heap.alloc(Struct::new(attrs)))
    }

    /// Returns a List of all the sources used by this node.
    fn sources(this: &StarlarkConfiguredTargetNode) -> anyhow::Result<Vec<StarlarkArtifact>> {
        struct InputsCollector {
            inputs: Vec<StarlarkArtifact>,
        }
        impl<'a> ConfiguredAttrTraversal<'a> for InputsCollector {
            fn dep(&mut self, _dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
                Ok(())
            }

            fn input(&mut self, path: &'a BuckPath) -> anyhow::Result<()> {
                self.inputs
                    .push(StarlarkArtifact::new(Artifact::from(SourceArtifact::new(
                        path.clone(),
                    ))));
                Ok(())
            }
        }
        let mut traversal = InputsCollector { inputs: Vec::new() };
        for (_, attr) in this.0.attrs() {
            attr.traverse(&mut traversal)?;
        }
        Ok(traversal.inputs)
    }

    /// Gets the `SourceArtifact` that corresponds to the given `path` given a context
    fn get_source(
        this: &StarlarkConfiguredTargetNode,
        path: &str,
        ctx: &BxlContext,
    ) -> anyhow::Result<Option<StarlarkArtifact>> {
        let path = Path::new(path);
        let fs = ctx.async_ctx.0.global_data().get_io_provider().fs().dupe();
        let path = if path.is_absolute() {
            fs.relativize(AbsPath::new(path)?)
                .context("Given path does not belong to the project root")?
        } else {
            Cow::Borrowed(ProjectRelativePath::new(path).context(
                "Given path should either be absolute or a forward pointing project relative path",
            )?)
        };

        let cell_path = ctx
            .async_ctx
            .via_dice(async move |ctx| ctx.get_cell_resolver().await.get_cell_path(&path))?;

        struct SourceFinder {
            found: Option<StarlarkArtifact>,
            target: CellPath,
        }
        impl<'a> ConfiguredAttrTraversal<'a> for SourceFinder {
            fn dep(&mut self, _dep: &'a ConfiguredProvidersLabel) -> anyhow::Result<()> {
                Ok(())
            }

            fn input(&mut self, path: &'a BuckPath) -> anyhow::Result<()> {
                if path.to_cell_path() == self.target {
                    self.found = Some(StarlarkArtifact::new(Artifact::from(SourceArtifact::new(
                        path.clone(),
                    ))));
                }
                Ok(())
            }
        }
        let mut traversal = SourceFinder {
            found: None,
            target: cell_path,
        };
        for (_, attr) in this.0.attrs() {
            attr.traverse(&mut traversal)?;

            if let Some(found) = traversal.found {
                return Ok(Some(found));
            }
        }
        Ok(None)
    }
}

impl<'v> AllocValue<'v> for ConfiguredTargetNode {
    fn alloc_value(self, heap: &'v starlark::values::Heap) -> Value<'v> {
        heap.alloc_simple(StarlarkConfiguredTargetNode(self))
    }
}

#[derive(Debug, Clone, Display, ProvidesStaticType, NoSerialize)]
#[display(fmt = "Traversal({})", self.0)]
#[repr(C)]
pub struct StarlarkConfiguredValue(ConfiguredAttr);

starlark_simple_value!(StarlarkConfiguredValue);

impl<'v> StarlarkValue<'v> for StarlarkConfiguredValue {
    starlark_type!("configured_attr_val");

    fn get_methods(&self) -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(configured_value_methods)
    }
}

#[starlark_module]
fn configured_value_methods(builder: &mut MethodsBuilder) {
    /// returns the type name of the attribute
    #[starlark(attribute)]
    fn r#type<'v>(this: &StarlarkConfiguredValue) -> anyhow::Result<&'v str> {
        Ok(this.0.type_name())
    }
}
