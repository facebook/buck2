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

use allocative::Allocative;
use anyhow::Context;
use buck2_build_api::actions::artifact::Artifact;
use buck2_build_api::analysis::calculation::get_dep_analysis;
use buck2_build_api::analysis::calculation::resolve_queries;
use buck2_build_api::analysis::get_deps_from_analysis_results;
use buck2_build_api::analysis::AnalysisResult;
use buck2_build_api::analysis::RuleAnalysisAttrResolutionContext;
use buck2_build_api::attrs::resolve::configured_attr::ConfiguredAttrExt;
use buck2_build_api::interpreter::rule_defs::artifact::StarlarkArtifact;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_core::buck_path::BuckPathRef;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::fs::paths::abs_norm_path::AbsNormPath;
use buck2_core::fs::project::ProjectRelativePath;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::ConfiguredTargetLabel;
use buck2_execute::artifact::source_artifact::SourceArtifact;
use buck2_interpreter::types::target_label::StarlarkConfiguredTargetLabel;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use buck2_node::attrs::configured_traversal::ConfiguredAttrTraversal;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use derivative::Derivative;
use derive_more::Display;
use gazebo::any::ProvidesStaticType;
use gazebo::prelude::*;
use once_cell::sync::OnceCell;
use starlark::collections::SmallMap;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::starlark_type;
use starlark::values::structs::Struct;
use starlark::values::type_repr::StarlarkTypeRepr;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::StarlarkDocs;

use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::nodes::configured::attr_resolution_ctx::LazyAttrResolutionContext;

mod attr_resolution_ctx;

#[derive(Debug, Display, ProvidesStaticType, StarlarkDocs, Allocative)]
#[derive(NoSerialize)] // TODO probably should be serializable the same as how queries serialize
#[display(fmt = "configured_target_node(name = {}, ...)", "self.0.name()")]
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
    /// attributes names, and the values are [`StarlarkConfiguredValue`].
    ///
    /// If you need to access many or all attrs on the same node, then this is the preferred way. Otherwise,

    /// using `attrs_lazy()` would be a better option for only accessing only a few attrs, although this really
    /// depends on what kind of attrs are on the node. Benchmarking performance will give you the best
    /// indication on which method to use.
    ///
    /// You should store the result of this function call for further usage in the code rather than calling

    /// `attrs()` each time you need to access the attrs.
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

    /// Gets a `StarlarkLazyAttrs` for getting attrs lazily. Returns a `StarlarkLazyAttrs` object
    /// that you can call `get()` on that gets an attr one at a time.
    ///
    /// If you need to access only few attrs on the same node, then this is the preferred way. Otherwise,

    /// using `attrs()` would be a better option for accessing many or all attrs, although this really
    /// depends on what kind of attrs are on the node. Benchmarking performance will give you the best
    /// indication on which method to use.
    ///
    /// You should store the result of this function call for further usage in the code rather than calling

    /// `attrs_lazy()` each time to get the `StarlarkLazyAttrs` object.
    fn attrs_lazy<'v>(
        this: &'v StarlarkConfiguredTargetNode,
    ) -> anyhow::Result<StarlarkLazyAttrs<'v>> {
        Ok(StarlarkLazyAttrs::new(this))
    }

    /// Gets a `StarlarkLazyResolvedAttrs` for getting resolved attrs lazily. Returns a `StarlarkLazyResolvedAttrs` object
    /// that you can call `get()` on that gets a resolved attr one at a time.
    ///
    /// If you need to access only few resolved attrs on the same node, then this is the preferred way. Otherwise,

    /// using `resolved_attrs()` would be a better option for accessing many or all resolved attrs, although this really
    /// depends on what kind of resolved attrs are on the node. Benchmarking performance will give you the best
    /// indication on which method to use.
    ///
    /// You should store the result of this function call for further usage in the code rather than calling

    /// `resolved_attrs_lazy()` each time to get the `StarlarkResolvedLazyAttrs` object.
    fn resolved_attrs_lazy<'v>(
        this: &'v StarlarkConfiguredTargetNode,
        ctx: &'v BxlContext<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<StarlarkLazyResolvedAttrs<'v>> {
        Ok(StarlarkLazyResolvedAttrs::new(this, ctx, eval.module()))
    }

    /// Returns a struct of all the resolved attributes of this target node. The structs fields are the
    /// attributes names, and the values are Starlark `[Value]`.
    ///
    /// If you need to access many or all resolved attrs on the same node, then this is the preferred way. Otherwise,

    /// using `resolved_attrs_lazy()` would be a better option for accessing only a few resolved attrs, although this really
    /// depends on what kind of resolved attrs are on the node. Benchmarking performance will give you the best
    /// indication on which method to use.
    ///
    /// You should store the result of this function call for further usage in the code rather than calling

    /// `resolved_attrs()` each time you need all the resolved attrs.
    fn resolved_attrs<'v>(
        this: &'v StarlarkConfiguredTargetNode,
        ctx: &'v BxlContext<'v>,
        eval: &mut Evaluator<'v, '_>,
    ) -> anyhow::Result<Value<'v>> {
        let configured_node = &this.0;

        let dep_analysis: anyhow::Result<Vec<(&ConfiguredTargetLabel, AnalysisResult)>, _> = ctx
            .async_ctx
            .via_dice(|dice_ctx| async move { get_dep_analysis(configured_node, dice_ctx).await });

        let query_results = ctx
            .async_ctx
            .via_dice(|dice_ctx| async move { resolve_queries(dice_ctx, configured_node).await })?;

        let resolution_ctx = RuleAnalysisAttrResolutionContext {
            module: eval.module(),
            dep_analysis_results: get_deps_from_analysis_results(dep_analysis?)?,
            query_results,
        };

        let attrs_iter = this.0.attrs(AttrInspectOptions::All);
        let mut resolved_attrs = SmallMap::with_capacity(attrs_iter.size_hint().0);

        for (name, attr) in attrs_iter {
            resolved_attrs.insert(
                eval.heap().alloc_str(name),
                attr.resolve_single(&resolution_ctx)?,
            );
        }

        Ok(eval.heap().alloc(Struct::new(resolved_attrs)))
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

            fn input(&mut self, path: BuckPathRef) -> anyhow::Result<()> {
                self.inputs
                    .push(StarlarkArtifact::new(Artifact::from(SourceArtifact::new(
                        path.to_buck_path(),
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
        let fs = ctx
            .async_ctx
            .0
            .global_data()
            .get_io_provider()
            .project_root()
            .dupe();
        let path = if path.is_absolute() {
            fs.relativize(AbsNormPath::new(path)?)
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

            fn input(&mut self, path: BuckPathRef) -> anyhow::Result<()> {
                if path.to_cell_path() == self.target {
                    self.found = Some(StarlarkArtifact::new(Artifact::from(SourceArtifact::new(
                        path.to_buck_path(),
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

#[derive(
    Debug,
    Clone,
    Display,
    ProvidesStaticType,
    NoSerialize,
    StarlarkDocs,
    Allocative
)]
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
    /// Returns the type name of the attribute
    #[starlark(attribute)]
    fn r#type<'v>(this: &StarlarkConfiguredValue) -> anyhow::Result<&'v str> {
        this.0.starlark_type()
    }

    /// Returns the value of this attribute. The value here is not fully resolved like in rules.
    fn value<'v>(this: &StarlarkConfiguredValue, heap: &'v Heap) -> anyhow::Result<Value<'v>> {
        this.0.to_value(heap)
    }
}

/// The context for getting attrs lazily on a `StarlarkConfiguredTargetNode`.
#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    StarlarkDocs,
    Allocative
)]
#[starlark_docs_attrs(directory = "bxl")]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
pub struct StarlarkLazyAttrs<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    configured_target_node: &'v StarlarkConfiguredTargetNode,
}

impl<'v> StarlarkValue<'v> for StarlarkLazyAttrs<'v> {
    starlark_type!("lazy_attrs");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(lazy_attrs_methods)
    }
}

impl<'v> AllocValue<'v> for StarlarkLazyAttrs<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> StarlarkTypeRepr for &'v StarlarkLazyAttrs<'v> {
    fn starlark_type_repr() -> String {
        StarlarkLazyAttrs::get_type_starlark_repr()
    }
}

impl<'v> UnpackValue<'v> for &'v StarlarkLazyAttrs<'v> {
    fn unpack_value(x: Value<'v>) -> Option<&'v StarlarkLazyAttrs<'v>> {
        x.downcast_ref()
    }
}

impl<'v> StarlarkLazyAttrs<'v> {
    pub fn new(configured_target_node: &'v StarlarkConfiguredTargetNode) -> StarlarkLazyAttrs<'v> {
        Self {
            configured_target_node,
        }
    }
}

#[starlark_module]
fn lazy_attrs_methods(builder: &mut MethodsBuilder) {
    /// Gets a single attribute.
    fn get<'v>(
        this: &StarlarkLazyAttrs<'v>,
        attr: &str,
    ) -> anyhow::Result<Option<StarlarkConfiguredValue>> {
        Ok(this
            .configured_target_node
            .0
            .get(attr, AttrInspectOptions::All)
            .map(StarlarkConfiguredValue))
    }
}

/// The context for getting resolved attrs lazily on a `StarlarkConfiguredTargetNode`.
#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    StarlarkDocs,
    Allocative
)]
#[starlark_docs_attrs(directory = "bxl")]
#[derivative(Debug)]
#[display(fmt = "{:?}", self)]
pub struct StarlarkLazyResolvedAttrs<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    configured_node: &'v ConfiguredTargetNode,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    resolution_ctx: LazyAttrResolutionContext<'v>,
}

impl<'v> StarlarkValue<'v> for StarlarkLazyResolvedAttrs<'v> {
    starlark_type!("lazy_resolved_attrs");

    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(lazy_resolved_attrs_methods)
    }
}

impl<'v> AllocValue<'v> for StarlarkLazyResolvedAttrs<'v> {
    fn alloc_value(self, heap: &'v Heap) -> Value<'v> {
        heap.alloc_complex_no_freeze(self)
    }
}

impl<'v> StarlarkTypeRepr for &'v StarlarkLazyResolvedAttrs<'v> {
    fn starlark_type_repr() -> String {
        StarlarkLazyResolvedAttrs::get_type_starlark_repr()
    }
}

impl<'v> UnpackValue<'v> for &'v StarlarkLazyResolvedAttrs<'v> {
    fn unpack_value(x: Value<'v>) -> Option<&'v StarlarkLazyResolvedAttrs<'v>> {
        x.downcast_ref()
    }
}

impl<'v> StarlarkLazyResolvedAttrs<'v> {
    pub fn new(
        configured_node: &'v StarlarkConfiguredTargetNode,
        ctx: &'v BxlContext<'v>,
        module: &'v Module,
    ) -> StarlarkLazyResolvedAttrs<'v> {
        let configured_node = &configured_node.0;
        let resolution_ctx = LazyAttrResolutionContext {
            module,
            configured_node,
            ctx,
            dep_analysis_results: OnceCell::new(),
            query_results: OnceCell::new(),
        };

        Self {
            configured_node,
            resolution_ctx,
        }
    }
}

#[starlark_module]
fn lazy_resolved_attrs_methods(builder: &mut MethodsBuilder) {
    /// Gets a single resolved attribute.
    fn get<'v>(
        this: &StarlarkLazyResolvedAttrs<'v>,
        attr: &str,
    ) -> anyhow::Result<Option<Value<'v>>> {
        Ok(
            match this.configured_node.get(attr, AttrInspectOptions::All) {
                Some(attr) => Some(attr.resolve_single(&this.resolution_ctx)?),
                None => None,
            },
        )
    }
}
