/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::path::Path;

use anyhow::Context;
use buck2_build_api::actions::artifact::Artifact;
use buck2_build_api::attrs::resolve::configured_attr::ConfiguredAttrExt;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkArtifact;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_core::buck_path::BuckPath;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::fs::paths::AbsPath;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_execute::artifact::source_artifact::SourceArtifact;
use buck2_interpreter::types::target_label::StarlarkConfiguredTargetLabel;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use buck2_node::attrs::configured_traversal::ConfiguredAttrTraversal;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use gazebo::prelude::*;
use starlark::collections::SmallMap;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::starlark_type;
use starlark::values::structs::Struct;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::StarlarkDocs;

use crate::bxl::starlark_defs::context::BxlContext;

#[derive(Debug, Display, ProvidesStaticType, StarlarkDocs)]
#[derive(NoSerialize)] // TODO probably should be serializable the same as how queries serialize
#[display(fmt = "{:?}", self)]
#[starlark_docs_attrs(directory = "bxl")]
pub struct StarlarkConfiguredTargetNode(pub ConfiguredTargetNode);

starlark_simple_value!(StarlarkConfiguredTargetNode);

impl<'v> StarlarkValue<'v> for StarlarkConfiguredTargetNode {
    starlark_type!("target_node");

    fn get_methods() -> Option<&'static Methods> {
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
    /// Gets the configured target label of this target node
    #[starlark(attribute)]
    fn label(this: &StarlarkConfiguredTargetNode) -> anyhow::Result<StarlarkConfiguredTargetLabel> {
        Ok(StarlarkConfiguredTargetLabel::new(this.0.name().dupe()))
    }

    /// Returns a struct of all the attributes of this target node. The structs fields are the
    /// attributes names, and the values are [`StarlarkConfiguredValue`]
    fn attrs<'v>(this: &StarlarkConfiguredTargetNode, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        let attrs_iter = this.0.attrs(AttrInspectOptions::All);
        let mut attrs = SmallMap::with_capacity(attrs_iter.size_hint().0);
        for (name, attr) in attrs_iter {
            attrs.insert(
                heap.alloc_str(name),
                heap.alloc(StarlarkConfiguredValue(attr)),
            );
        }

        Ok(heap.alloc(Struct::new(attrs)))
    }

    /// Gets the targets' corresponding rule's name. This is the fully qualified rule name including
    /// the import path.
    #[starlark(attribute)]
    fn rule_type(this: &StarlarkConfiguredTargetNode) -> anyhow::Result<String> {
        Ok(this.0.rule_type().to_string())
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
        for (_, attr) in this.0.attrs(AttrInspectOptions::All) {
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
            .via_dice(async move |ctx| ctx.get_cell_resolver().await?.get_cell_path(&path))?;

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
        for (_, attr) in this.0.attrs(AttrInspectOptions::All) {
            attr.traverse(&mut traversal)?;

            if let Some(found) = traversal.found {
                return Ok(Some(found));
            }
        }
        Ok(None)
    }
}

#[derive(Debug, Clone, Display, ProvidesStaticType, NoSerialize, StarlarkDocs)]
#[display(fmt = "Traversal({})", self.0)]
#[repr(C)]
#[starlark_docs_attrs(directory = "bxl")]
pub struct StarlarkConfiguredValue(ConfiguredAttr);

starlark_simple_value!(StarlarkConfiguredValue);

impl<'v> StarlarkValue<'v> for StarlarkConfiguredValue {
    starlark_type!("configured_attr_val");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(configured_value_methods)
    }
}

#[starlark_module]
fn configured_value_methods(builder: &mut MethodsBuilder) {
    /// returns the type name of the attribute
    #[starlark(attribute)]
    fn r#type<'v>(this: &StarlarkConfiguredValue) -> anyhow::Result<&'v str> {
        this.0.starlark_type()
    }

    /// returns the value of this attribute. The value here is not fully resolved like in rules.
    fn value<'v>(this: &StarlarkConfiguredValue, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        this.0.to_value(heap)
    }
}
