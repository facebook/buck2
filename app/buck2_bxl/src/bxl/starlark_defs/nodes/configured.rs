/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::borrow::Cow;
use std::collections::HashMap;
use std::convert::Infallible;
use std::fmt;
use std::path::Path;
use std::sync::OnceLock;

use allocative::Allocative;
use buck2_analysis::analysis::calculation::get_dep_analysis;
use buck2_analysis::analysis::calculation::resolve_queries;
use buck2_analysis::analysis::env::RuleAnalysisAttrResolutionContext;
use buck2_analysis::analysis::env::get_deps_from_analysis_results;
use buck2_analysis::attrs::resolve::configured_attr::ConfiguredAttrExt;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::source_artifact::SourceArtifact;
use buck2_build_api::actions::query::PackageLabelOption;
use buck2_build_api::analysis::AnalysisResult;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use buck2_common::dice::cells::HasCellResolver;
use buck2_common::dice::data::HasIoProvider;
use buck2_core::cells::cell_path::CellPath;
use buck2_core::fs::paths::abs_path::AbsPath;
use buck2_core::fs::project_rel_path::ProjectRelativePath;
use buck2_core::package::PackageLabel;
use buck2_core::package::source_path::SourcePathRef;
use buck2_core::provider::label::ConfiguredProvidersLabel;
use buck2_core::target::configured_target_label::ConfiguredTargetLabel;
use buck2_error::BuckErrorContext;
use buck2_interpreter::types::target_label::StarlarkConfiguredTargetLabel;
use buck2_node::attrs::configured_attr::ConfiguredAttr;
use buck2_node::attrs::configured_traversal::ConfiguredAttrTraversal;
use buck2_node::attrs::display::AttrDisplayWithContext;
use buck2_node::attrs::fmt_context::AttrFmtContext;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::attrs::serialize::AttrSerializeWithContext;
use buck2_node::nodes::configured::ConfiguredTargetNode;
use derivative::Derivative;
use derive_more::Display;
use dupe::Dupe;
use futures::FutureExt;
use serde::Serialize;
use serde::Serializer;
use starlark::any::ProvidesStaticType;
use starlark::collections::SmallMap;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::environment::Module;
use starlark::eval::Evaluator;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::AllocValue;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::Trace;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::ValueTyped;
use starlark::values::list::AllocList;
use starlark::values::none::NoneOr;
use starlark::values::starlark_value;
use starlark::values::structs::AllocStruct;

use super::node_attrs::NodeAttributeGetter;
use crate::bxl::starlark_defs::context::BxlContext;
use crate::bxl::starlark_defs::file_set::StarlarkFileNode;
use crate::bxl::starlark_defs::nodes::configured::attr_resolution_ctx::LazyAttrResolutionContext;

mod attr_resolution_ctx;

#[derive(Debug, Display, ProvidesStaticType, Allocative, Clone, Dupe)]
#[derive(NoSerialize)] // TODO probably should be serializable the same as how queries serialize
#[display("configured_target_node(name = {}, ...)", self.0.label())]
pub(crate) struct StarlarkConfiguredTargetNode(pub(crate) ConfiguredTargetNode);

starlark_simple_value!(StarlarkConfiguredTargetNode);

#[starlark_value(type = "bxl.ConfiguredTargetNode")]
impl<'v> StarlarkValue<'v> for StarlarkConfiguredTargetNode {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(configured_target_node_value_methods)
    }
}

impl<'a> UnpackValue<'a> for StarlarkConfiguredTargetNode {
    type Error = Infallible;

    fn unpack_value_impl(value: Value<'a>) -> Result<Option<Self>, Self::Error> {
        Ok(value
            .downcast_ref::<Self>()
            .map(|value| Self(value.0.dupe())))
    }
}

#[starlark_module]
fn configured_target_node_value_methods(builder: &mut MethodsBuilder) {
    /// Gets the configured target label of this target node.
    ///
    /// Note that you cannot get a non-configured label from a configured target node because the
    /// configured target node is not uniquely identified a non-configured label, only by the configured target label.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_label(ctx):
    ///     node = ctx.configured_targets("my_cell//bin:the_binary")
    ///     ctx.output.print(node.label)
    /// ```
    #[starlark(attribute)]
    fn label(
        this: &StarlarkConfiguredTargetNode,
    ) -> starlark::Result<StarlarkConfiguredTargetLabel> {
        Ok(StarlarkConfiguredTargetLabel::new(this.0.label().dupe()))
    }

    /// Gets the buildfile path from the configured target node.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_label(ctx):
    ///     target_node = ctx.cquery().eval("owner('path/to/file')")[0]
    ///     ctx.output.print(target_node.buildfile_path)
    /// ```
    #[starlark(attribute)]
    fn buildfile_path(this: &StarlarkConfiguredTargetNode) -> starlark::Result<StarlarkFileNode> {
        Ok(StarlarkFileNode(this.0.buildfile_path().path()))
    }

    /// Gets the attribute from the configured target node.
    /// If the attribute is unset, returns the default value.
    /// If the attribute is not defined by the rule, returns `None`.
    /// It will not return special attribute (attribute that start with 'buck.' in `buck2 cquery -A` command).
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_attributes(ctx):
    ///     target_node = ctx.uquery().eval("//foo:bar")[0]
    ///     ctx.output.print(target_node.get_attr('my_attr'))
    /// ```
    fn get_attr<'v>(
        this: &StarlarkConfiguredTargetNode,
        #[starlark(require=pos)] key: &str,
        heap: &'v Heap,
    ) -> starlark::Result<NoneOr<Value<'v>>> {
        Ok(NodeAttributeGetter::get_attr(this, key, heap)?)
    }

    /// Gets the all attributes (not include speical attributes) from the configured target node.
    /// For attributes that are not explicitly set, the default value is returned.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_attributes(ctx):
    ///     target_node = ctx.uquery().eval("//foo:bar")[0]
    ///     ctx.output.print(target_node.get_attrs())
    /// ```
    fn get_attrs<'v>(
        this: &StarlarkConfiguredTargetNode,
        heap: &'v Heap,
    ) -> starlark::Result<SmallMap<StringValue<'v>, Value<'v>>> {
        Ok(NodeAttributeGetter::get_attrs(this, heap)?)
    }

    /// Check if rule has the attribute.
    ///
    /// Known attribute is always set explicitly or to default value
    /// (otherwise target would not be created)
    /// For special attributes, it will return `False`
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_has_attr(ctx):
    ///     target_node = ctx.uquery().eval("//foo:bar")[0]
    ///     ctx.output.print(target_node.has_attr('my_attr'))
    /// ```
    fn has_attr<'v>(
        this: &StarlarkConfiguredTargetNode,
        #[starlark(require=pos)] key: &str,
    ) -> starlark::Result<bool> {
        Ok(NodeAttributeGetter::has_attr(this, key))
    }

    /// Returns a struct of all the attributes of this target node. The structs fields are the
    /// attributes names, and the values are [`StarlarkConfiguredAttr`].
    ///
    /// If you need to access many or all attrs on the same node, then this is the preferred way. Otherwise,
    /// using `attrs_lazy()` would be a better option for only accessing only a few attrs, although this really
    /// depends on what kind of attrs are on the node. Benchmarking performance will give you the best
    /// indication on which method to use.
    ///
    /// You should store the result of this function call for further usage in the code rather than calling
    /// `attrs_eager()` each time you need to access the attrs.
    ///
    /// Right now, it is not recommended to use this method. Instead, use `get_attr` and `get_attrs` methods.
    /// We will deprecate this method in the future.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_attrs_eager(ctx):
    ///     node = ctx.cquery().owner("cell//path/to/TARGETS")[0]
    ///     attrs = node.attrs_eager() # cache once
    ///     ctx.output.print(attrs)
    ///     # do more stuff with attrs
    /// ```
    fn attrs_eager<'v>(
        this: &StarlarkConfiguredTargetNode,
        heap: &'v Heap,
    ) -> starlark::Result<Value<'v>> {
        let attrs_iter = this.0.attrs(AttrInspectOptions::All);
        let special_attrs_iter = this.0.special_attrs();

        let attrs = attrs_iter
            .map(|a| {
                (
                    a.name,
                    StarlarkConfiguredAttr(a.value, this.0.label().pkg().dupe()),
                )
            })
            .chain(special_attrs_iter.map(|(name, attr)| {
                (
                    name,
                    StarlarkConfiguredAttr(attr, this.0.label().pkg().dupe()),
                )
            }));

        Ok(heap.alloc(AllocStruct(attrs)))
    }

    /// Returns a `lazy_attrs` object that you can call `get()` on that gets an attr one at a time.
    ///
    /// If you need to access only few attrs on the same node, then this is the preferred way. Otherwise,
    /// using `attrs_eager()` would be a better option for accessing many or all attrs, although this really
    /// depends on what kind of attrs are on the node. Benchmarking performance will give you the best
    /// indication on which method to use.
    ///
    /// You should store the result of this function call for further usage in the code rather than calling
    /// `attrs_lazy()` each time to get the `lazy_attrs` object. Note that if the `get()` is `None`,
    /// then any methods called on `None` will result in an error.
    ///
    /// Right now, it is not recommended to use this method. Instead, use `get_attr` and `get_attrs` methods.
    /// We will deprecate this method in the future.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_attrs_lazy(ctx):
    ///     node = ctx.cquery().owner("cell//path/to/TARGETS")[0]
    ///     attrs = node.attrs_lazy() # cache once
    ///     ctx.output.print(attrs.get("some_attributes").value())
    ///     ctx.output.print(attrs.get("some_attribute").label)
    /// ```
    fn attrs_lazy<'v>(
        this: &'v StarlarkConfiguredTargetNode,
    ) -> starlark::Result<StarlarkLazyAttrs<'v>> {
        Ok(StarlarkLazyAttrs::new(this))
    }

    /// Returns a `lazy_resolved_attrs` object that you can call `get()` on that gets a resolved attr one at a time.
    ///
    /// If you need to access only few resolved attrs on the same node, then this is the preferred way. Otherwise,
    /// using `resolved_attrs_eager()` would be a better option for accessing many or all resolved attrs, although this really
    /// depends on what kind of resolved attrs are on the node. Benchmarking performance will give you the best
    /// indication on which method to use.
    ///
    /// You should store the result of this function call for further usage in the code rather than calling
    /// `resolved_attrs_lazy()` each time to get the `lazy_resolved_attrs` object. Note that if the `get()` is `None`,
    /// then any methods called on `None` will result in an error.
    ///
    /// Right now, it is not recommended to use this method. Instead, use `get_attr` and `get_attrs` methods.
    /// We will deprecate this method in the future.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_resolved_attrs_lazy(ctx):
    ///     node = ctx.cquery().owner("cell//path/to/TARGETS")[0]
    ///     attrs = node.resolved_attrs_lazy(ctx) # cache once
    ///     ctx.output.print(attrs.get("some_attributes").value())
    ///     ctx.output.print(attrs.get("some_attribute").label)
    /// ```
    fn resolved_attrs_lazy<'v>(
        this: &'v StarlarkConfiguredTargetNode,
        ctx: &'v BxlContext<'v>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<StarlarkLazyResolvedAttrs<'v>> {
        Ok(StarlarkLazyResolvedAttrs::new(this, ctx, eval.module()))
    }

    /// Returns a struct of all the resolved attributes of this target node. The structs fields are the
    /// attributes names, and the values are the underlying Starlark values of the attributes.
    ///
    /// If you need to access many or all resolved attrs on the same node, then this is the preferred way. Otherwise,
    /// using `resolved_attrs_lazy()` would be a better option for accessing only a few resolved attrs, although this really
    /// depends on what kind of resolved attrs are on the node. Benchmarking performance will give you the best
    /// indication on which method to use.
    ///
    /// You should store the result of this function call for further usage in the code rather than calling
    /// `resolved_attrs_eager()` each time you need all the resolved attrs.
    ///
    /// Right now, it is not recommended to use this method. Instead, use `get_attr` and `get_attrs` methods.
    /// We will deprecate this method in the future.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_resolved_attrs_eager(ctx):
    ///     node = ctx.cquery().owner("cell//path/to/TARGETS")[0]
    ///     attrs = node.resolved_attrs_eager(ctx) # cache once
    ///     ctx.output.print(attrs)
    ///     # do more stuff with attrs
    /// ```
    fn resolved_attrs_eager<'v>(
        this: &'v StarlarkConfiguredTargetNode,
        ctx: &'v BxlContext<'v>,
        eval: &mut Evaluator<'v, '_, '_>,
    ) -> starlark::Result<Value<'v>> {
        let configured_node = this.0.as_ref();

        let dep_analysis: buck2_error::Result<Vec<(&ConfiguredTargetLabel, AnalysisResult)>> = ctx
            .async_ctx
            .borrow_mut()
            .via(|dice_ctx| get_dep_analysis(configured_node, dice_ctx).boxed_local());

        let query_results = ctx
            .async_ctx
            .borrow_mut()
            .via(|dice_ctx| resolve_queries(dice_ctx, configured_node).boxed_local())?;

        let resolution_ctx = RuleAnalysisAttrResolutionContext {
            module: eval.module(),
            dep_analysis_results: get_deps_from_analysis_results(dep_analysis?)?,
            query_results,
            execution_platform_resolution: configured_node.execution_platform_resolution().clone(),
        };

        let attrs_iter = this.0.attrs(AttrInspectOptions::All);
        let mut resolved_attrs = Vec::with_capacity(attrs_iter.size_hint().0);

        for a in attrs_iter {
            resolved_attrs.push((
                a.name,
                a.value
                    .resolve_single(this.0.label().pkg(), &resolution_ctx)?,
            ));
        }

        Ok(eval.heap().alloc(AllocStruct(resolved_attrs)))
    }

    /// Skip incoming transition forward node.
    /// If a target is a forward node, which is created by applying incoming configuration transition,
    /// return the transition target, otherwise return itself.
    /// This is is particularly useful when you don't care about 'forward' node.
    ///
    /// Example usage:
    /// ```python
    /// def _impl_unwrap_forward(ctx):
    ///     node = ctx.configured_targets("my_cell//bin:the_binary")
    ///     actual_node = node.unwrap_forward()
    /// ```
    fn unwrap_forward<'v>(
        this: ValueTyped<'v, StarlarkConfiguredTargetNode>,
        heap: &'v Heap,
    ) -> starlark::Result<ValueTyped<'v, StarlarkConfiguredTargetNode>> {
        match this.0.forward_target() {
            Some(n) => Ok(heap.alloc_typed(StarlarkConfiguredTargetNode(n.dupe()))),
            None => Ok(this),
        }
    }

    /// Gets the targets' corresponding rule's name. This is the fully qualified rule name including
    /// the import path.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_rule_type(ctx):
    ///     node = ctx.configured_targets("my_cell//bin:the_binary")
    ///     ctx.output.print(node.rule_type)
    /// ```
    #[starlark(attribute)]
    fn rule_type<'v>(
        this: &'v StarlarkConfiguredTargetNode,
        heap: &'v Heap,
    ) -> starlark::Result<StringValue<'v>> {
        Ok(heap.alloc_str_intern(this.0.rule_type().to_string().as_str()))
    }

    /// Gets the targets' corresponding rule's kind which is one of
    ///  - normal (with no special properties)
    ///  - configured (usable in a configuration context)
    ///  - toolchain (only usable as a toolchain dep)
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_rule_kind(ctx):
    ///     node = ctx.configured_targets("my_cell//bin:the_binary")
    ///     ctx.output.print(node.rule_kind)
    /// ```
    #[starlark(attribute)]
    fn rule_kind<'v>(
        this: &'v StarlarkConfiguredTargetNode,
        heap: &'v Heap,
    ) -> starlark::Result<StringValue<'v>> {
        Ok(heap.alloc_str_intern(this.0.rule_kind().as_str()))
    }

    /// Returns all source `Artifact`s exist in this target's attributes.
    /// This method will traverse all the attributes to find and collect all the source `Artifact`s.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_sources(ctx):
    ///     node = ctx.configured_targets("my_cell//bin:the_binary")
    ///     ctx.output.print(node.sources())
    /// ```
    fn sources(this: &StarlarkConfiguredTargetNode) -> starlark::Result<Vec<StarlarkArtifact>> {
        struct InputsCollector {
            inputs: Vec<StarlarkArtifact>,
        }
        impl ConfiguredAttrTraversal for InputsCollector {
            fn dep(&mut self, _dep: &ConfiguredProvidersLabel) -> buck2_error::Result<()> {
                Ok(())
            }

            fn input(&mut self, path: SourcePathRef) -> buck2_error::Result<()> {
                self.inputs
                    .push(StarlarkArtifact::new(Artifact::from(SourceArtifact::new(
                        path.to_owned(),
                    ))));
                Ok(())
            }
        }
        let mut traversal = InputsCollector { inputs: Vec::new() };
        for a in this.0.attrs(AttrInspectOptions::All) {
            a.traverse(this.0.label().pkg(), &mut traversal)?;
        }
        Ok(traversal.inputs)
    }

    /// Gets the source `Artifact` that corresponds to the given `path` given a context. The path should be the
    /// project relative path to the file, or an absolute path.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_get_source(ctx):
    ///     owner = ctx.cquery().owner("project/relative/path/to/file")[0]
    ///     artifact = owner.sources()[0]
    ///     ctx.output.print(artifact)
    /// ```
    fn get_source(
        this: &StarlarkConfiguredTargetNode,
        path: &str,
        ctx: &BxlContext,
    ) -> starlark::Result<NoneOr<StarlarkArtifact>> {
        let path = Path::new(path);
        let fs = ctx
            .async_ctx
            .borrow()
            .global_data()
            .get_io_provider()
            .project_root()
            .dupe();
        let path = if path.is_absolute() {
            Cow::Owned(
                fs.relativize_any(AbsPath::new(path)?)
                    .buck_error_context_anyhow("Given path does not belong to the project root")?,
            )
        } else {
            Cow::Borrowed(ProjectRelativePath::new(path).buck_error_context_anyhow(
                "Given path should either be absolute or a forward pointing project relative path",
            )?)
        };

        let cell_path = ctx.async_ctx.borrow_mut().via(|ctx| {
            async move { ctx.get_cell_resolver().await?.get_cell_path(&path) }.boxed_local()
        })?;

        struct SourceFinder {
            found: Option<StarlarkArtifact>,
            target: CellPath,
        }
        impl ConfiguredAttrTraversal for SourceFinder {
            fn dep(&mut self, _dep: &ConfiguredProvidersLabel) -> buck2_error::Result<()> {
                Ok(())
            }

            fn input(&mut self, path: SourcePathRef) -> buck2_error::Result<()> {
                if path.to_cell_path() == self.target {
                    self.found = Some(StarlarkArtifact::new(Artifact::from(SourceArtifact::new(
                        path.to_owned(),
                    ))));
                }
                Ok(())
            }
        }
        let mut traversal = SourceFinder {
            found: None,
            target: cell_path,
        };
        for a in this.0.attrs(AttrInspectOptions::All) {
            a.traverse(this.0.label().pkg(), &mut traversal)?;

            if let Some(found) = traversal.found {
                return Ok(NoneOr::Other(found));
            }
        }
        Ok(NoneOr::None)
    }

    /// Gets the target's special attr `oncall`
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_get_oncall(ctx):
    ///     target_node = ctx.cquery().eval("//foo:bar")[0]
    ///     ctx.output.print(target_node.oncall)
    /// ```
    #[starlark(attribute)]
    fn oncall<'v>(
        this: &'v StarlarkConfiguredTargetNode,
        heap: &'v Heap,
    ) -> starlark::Result<NoneOr<StringValue<'v>>> {
        match this.0.oncall() {
            Some(oncall) => Ok(NoneOr::Other(heap.alloc_str_intern(oncall))),
            None => Ok(NoneOr::None),
        }
    }

    /// Gets all deps for this target.
    /// The result is a list of `ConfiguredTargetNode`.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_get_deps(ctx):
    ///     target_node = ctx.uquery().eval("//foo:bar")[0]
    ///     ctx.output.print(target_node.deps())
    /// ```
    fn deps<'v>(
        this: &'v StarlarkConfiguredTargetNode,
        // ) -> buck2_error::Result<Vec<StarlarkConfiguredTargetNode>> {
    ) -> starlark::Result<AllocList<impl IntoIterator<Item = StarlarkConfiguredTargetNode> + 'v>>
    {
        Ok(AllocList(
            this.0
                .deps()
                .map(|node| StarlarkConfiguredTargetNode(node.dupe()))
                .into_iter(),
        ))
    }
}

#[derive(Debug, Clone, ProvidesStaticType, Allocative)]
#[repr(C)]
pub(crate) struct StarlarkConfiguredAttr(ConfiguredAttr, PackageLabel);

impl Display for StarlarkConfiguredAttr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        self.0.fmt(
            &AttrFmtContext {
                package: Some(self.1.dupe()),
                options: Default::default(),
            },
            f,
        )
    }
}

impl Serialize for StarlarkConfiguredAttr {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: Serializer,
    {
        self.0.serialize_with_ctx(
            &AttrFmtContext {
                package: Some(self.1.dupe()),
                options: Default::default(),
            },
            serializer,
        )
    }
}

starlark_simple_value!(StarlarkConfiguredAttr);

#[starlark_value(type = "bxl.ConfiguredAttr")]
impl<'v> StarlarkValue<'v> for StarlarkConfiguredAttr {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(configured_attr_methods)
    }
}

/// Methods on configured target node's attributes.
#[starlark_module]
fn configured_attr_methods(builder: &mut MethodsBuilder) {
    /// Returns the type name of the attribute
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_type(ctx):
    ///     node = ctx.cquery().owner("bin/TARGETS")[0]
    ///     attrs = node.attrs_eager()
    ///     ctx.output.print(attrs.name.type)
    /// ```
    // FIXME(JakobDegen): Strings as types are mostly dead, users should be getting the value and
    // using `isinstance` instead. Remove this.
    #[starlark(attribute)]
    fn r#type<'v>(this: &StarlarkConfiguredAttr, heap: &'v Heap) -> starlark::Result<&'v str> {
        Ok(this
            .0
            .to_value(PackageLabelOption::PackageLabel(this.1.dupe()), heap)?
            .get_type())
    }

    /// Returns the value of this attribute. The value here is not fully resolved like in rules.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_value(ctx):
    ///     node = ctx.cquery().owner("bin/TARGETS")[0]
    ///     attrs = node.attrs_eager()
    ///     ctx.output.print(attrs.name.value())
    /// ```
    fn value<'v>(this: &StarlarkConfiguredAttr, heap: &'v Heap) -> starlark::Result<Value<'v>> {
        Ok(this
            .0
            .to_value(PackageLabelOption::PackageLabel(this.1.dupe()), heap)?)
    }
}

/// The context for getting attrs lazily on a `StarlarkConfiguredTargetNode`.
#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    Allocative
)]
#[derivative(Debug)]
#[display("{:?}", self)]
pub(crate) struct StarlarkLazyAttrs<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    configured_target_node: &'v StarlarkConfiguredTargetNode,
}

#[starlark_value(type = "bxl.LazyAttrs", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkLazyAttrs<'v> {
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

impl<'v> StarlarkLazyAttrs<'v> {
    pub(crate) fn new(
        configured_target_node: &'v StarlarkConfiguredTargetNode,
    ) -> StarlarkLazyAttrs<'v> {
        Self {
            configured_target_node,
        }
    }
}

/// The context for getting attrs lazily on a `target_node`.
#[starlark_module]
fn lazy_attrs_methods(builder: &mut MethodsBuilder) {
    /// Gets a single attribute. Returns an optional `[configured_attr]`.
    ///
    /// ```python
    /// def _impl_attrs_lazy(ctx):
    ///     node = ctx.cquery().owner("cell//path/to/TARGETS")[0]
    ///     attrs = node.attrs_lazy() # cache once
    ///     ctx.output.print(attrs.get("some_attributes").value())
    ///     ctx.output.print(attrs.get("some_attribute").label)
    /// ```
    fn get<'v>(
        this: &StarlarkLazyAttrs<'v>,
        attr: &str,
    ) -> starlark::Result<NoneOr<StarlarkConfiguredAttr>> {
        Ok(
            match this
                .configured_target_node
                .0
                .get(attr, AttrInspectOptions::All)
            {
                Some(attr) => NoneOr::Other(StarlarkConfiguredAttr(
                    attr.value,
                    this.configured_target_node.0.label().pkg().dupe(),
                )),
                None => {
                    // Check special attrs
                    let special_attrs = this
                        .configured_target_node
                        .0
                        .special_attrs()
                        .collect::<HashMap<_, _>>();
                    let attr = special_attrs.get(attr);
                    match attr {
                        None => NoneOr::None,
                        Some(attr) => NoneOr::Other(StarlarkConfiguredAttr(
                            attr.clone(),
                            this.configured_target_node.0.label().pkg().dupe(),
                        )),
                    }
                }
            },
        )
    }
}

#[derive(
    ProvidesStaticType,
    Derivative,
    Display,
    Trace,
    NoSerialize,
    Allocative
)]
#[derivative(Debug)]
#[display("{:?}", self)]
pub(crate) struct StarlarkLazyResolvedAttrs<'v> {
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    configured_node: &'v ConfiguredTargetNode,
    #[trace(unsafe_ignore)]
    #[derivative(Debug = "ignore")]
    #[allocative(skip)]
    resolution_ctx: LazyAttrResolutionContext<'v>,
}

#[starlark_value(type = "bxl.LazyResolvedAttrs", StarlarkTypeRepr, UnpackValue)]
impl<'v> StarlarkValue<'v> for StarlarkLazyResolvedAttrs<'v> {
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

impl<'v> StarlarkLazyResolvedAttrs<'v> {
    pub(crate) fn new(
        configured_node: &'v StarlarkConfiguredTargetNode,
        ctx: &'v BxlContext<'v>,
        module: &'v Module,
    ) -> StarlarkLazyResolvedAttrs<'v> {
        let configured_node = &configured_node.0;
        let resolution_ctx = LazyAttrResolutionContext {
            module,
            configured_node,
            ctx,
            dep_analysis_results: OnceLock::new(),
            query_results: OnceLock::new(),
        };

        Self {
            configured_node,
            resolution_ctx,
        }
    }
}

/// The context for getting resolved attrs lazily on a `target_node`.
#[starlark_module]
fn lazy_resolved_attrs_methods(builder: &mut MethodsBuilder) {
    /// Gets a single resolved attribute. Returns an optional configured attribute.
    ///
    /// Gets a single attribute.
    ///
    /// ```python
    /// def _impl_resolved_attrs_lazy(ctx):
    ///     node = ctx.cquery().owner("cell//path/to/TARGETS")[0]
    ///     attrs = node.resolved_attrs_lazy(ctx) # cache once
    ///     ctx.output.print(attrs.get("some_attribute").value())
    ///     ctx.output.print(attrs.get("some_attribute").label)
    /// ```
    fn get<'v>(
        this: &StarlarkLazyResolvedAttrs<'v>,
        attr: &str,
    ) -> starlark::Result<NoneOr<Value<'v>>> {
        Ok(
            match this.configured_node.get(attr, AttrInspectOptions::All) {
                Some(attr) => NoneOr::Other(
                    attr.value
                        .resolve_single(this.configured_node.label().pkg(), &this.resolution_ctx)?,
                ),
                None => {
                    // Check special attrs
                    let special_attrs = this
                        .configured_node
                        .special_attrs()
                        .collect::<HashMap<_, _>>();
                    let attr = special_attrs.get(attr);
                    match attr {
                        None => NoneOr::None,
                        Some(attr) => NoneOr::Other(attr.resolve_single(
                            this.configured_node.label().pkg(),
                            &this.resolution_ctx,
                        )?),
                    }
                }
            },
        )
    }
}
