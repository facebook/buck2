/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::convert::Infallible;

use allocative::Allocative;
use buck2_build_api::bxl::unconfigured_attribute::StarlarkCoercedAttr;
use buck2_interpreter::types::cell_path::StarlarkCellPath;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::nodes::unconfigured::TargetNode;
use derive_more::Display;
use dupe::Dupe;
use starlark::any::ProvidesStaticType;
use starlark::collections::SmallMap;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::Heap;
use starlark::values::NoSerialize;
use starlark::values::StarlarkValue;
use starlark::values::StringValue;
use starlark::values::UnpackValue;
use starlark::values::Value;
use starlark::values::ValueLike;
use starlark::values::list::AllocList;
use starlark::values::none::NoneOr;
use starlark::values::starlark_value;
use starlark::values::structs::AllocStruct;

use super::node_attrs::NodeAttributeGetter;
use crate::bxl::starlark_defs::file_set::StarlarkFileNode;

#[derive(Debug, Display, ProvidesStaticType, Allocative, Clone, Dupe)]
#[derive(NoSerialize)] // TODO probably should be serializable the same as how queries serialize
#[display("{:?}", self)]
pub(crate) struct StarlarkTargetNode(pub(crate) TargetNode);

starlark_simple_value!(StarlarkTargetNode);

#[starlark_value(type = "bxl.UnconfiguredTargetNode")]
impl<'v> StarlarkValue<'v> for StarlarkTargetNode {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(target_node_value_methods)
    }
}

impl<'a> UnpackValue<'a> for StarlarkTargetNode {
    type Error = Infallible;

    fn unpack_value_impl(value: Value<'a>) -> Result<Option<Self>, Self::Error> {
        Ok(value
            .downcast_ref::<Self>()
            .map(|value| Self(value.0.dupe())))
    }
}

/// Methods for unconfigured target node.
#[starlark_module]
fn target_node_value_methods(builder: &mut MethodsBuilder) {
    /// Gets the coerced attributes from the unconfigured target node. Returns a struct.
    /// Right now, it is not recommended to use this method. Instead, use `get_attr` and `get_attrs` methods.
    /// We will deprecate this method in the future.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_attributes(ctx):
    ///     target_node = ctx.uquery().eval("//foo:bar")[0]
    ///     ctx.output.print(target_node.attrs.my_attr)
    /// ```
    #[starlark(attribute)]
    fn attrs<'v>(this: StarlarkTargetNode, heap: Heap<'_>) -> starlark::Result<Value<'v>> {
        let attrs_iter = this.0.attrs(AttrInspectOptions::All);
        let special_attrs_iter = this.0.special_attrs();
        let attrs = attrs_iter
            .map(|a| {
                (
                    a.name,
                    StarlarkCoercedAttr(a.value.clone(), this.0.label().pkg().dupe()),
                )
            })
            .chain(special_attrs_iter.map(|(name, attr)| {
                (name, StarlarkCoercedAttr(attr, this.0.label().pkg().dupe()))
            }));

        Ok(heap.alloc(AllocStruct(attrs)))
    }

    /// Gets the attribute from the unconfigured target node.
    /// If the attribute is unset, returns the default value.
    /// If the attribute is not defined by the rule, returns `None`.
    /// It will not return special attribute (attribute that start with 'buck.' in `buck2 uquery -A` command).
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_attributes(ctx):
    ///     target_node = ctx.uquery().eval("//foo:bar")[0]
    ///     ctx.output.print(target_node.get_attr('my_attr'))
    /// ```
    fn get_attr<'v>(
        this: &StarlarkTargetNode,
        #[starlark(require=pos)] key: &str,
        heap: Heap<'v>,
    ) -> starlark::Result<NoneOr<Value<'v>>> {
        Ok(NodeAttributeGetter::get_attr(this, key, heap)?)
    }

    /// Gets all the attributes (excluding special attributes) from the unconfigured target node.
    /// For attributes that are not explicitly set, the default value is returned.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_attributes(ctx):
    ///     target_node = ctx.uquery().eval("//foo:bar")[0]
    ///     ctx.output.print(target_node.get_attrs())
    /// ```
    fn get_attrs<'v>(
        this: &StarlarkTargetNode,
        heap: Heap<'v>,
    ) -> starlark::Result<SmallMap<StringValue<'v>, Value<'v>>> {
        Ok(NodeAttributeGetter::get_attrs(this, heap)?)
    }

    /// Check if rule has the attribute.
    ///
    /// Known attribute is always set explicitly or to default value
    /// (otherwise target would not be created)
    /// For special attributes, it will return `False`
    ///
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_attributes(ctx):
    ///     target_node = ctx.uquery().eval("//foo:bar")[0]
    ///     ctx.output.print(target_node.has_attr('my_attr'))
    /// ```
    fn has_attr<'v>(
        this: &StarlarkTargetNode,
        #[starlark(require=pos)] key: &str,
    ) -> starlark::Result<bool> {
        Ok(NodeAttributeGetter::has_attr(this, key))
    }

    /// Gets the label from the unconfigured target node.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_label(ctx):
    ///     target_node = ctx.uquery().eval("//foo:bar")[0]
    ///     ctx.output.print(target_node.label)
    /// ```
    #[starlark(attribute)]
    fn label(this: &StarlarkTargetNode) -> starlark::Result<StarlarkTargetLabel> {
        Ok(this.0.label().dupe().into())
    }

    /// Gets the buildfile path from the unconfigured target node.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_label(ctx):
    ///     target_node = ctx.uquery().eval("//foo:bar")[0]
    ///     ctx.output.print(target_node.buildfile_path)
    /// ```
    #[starlark(attribute)]
    fn buildfile_path(this: &StarlarkTargetNode) -> starlark::Result<StarlarkFileNode> {
        Ok(StarlarkFileNode(this.0.buildfile_path().path()))
    }

    /// Gets the fully qualified name of the rule for this unconfigured target node as a
    /// string. This includes the import path as well.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_rule_type(ctx):
    ///     target_node = ctx.uquery().eval("//foo:bar")[0]
    ///     ctx.output.print(target_node.rule_type)
    /// ```
    #[starlark(attribute)]
    fn rule_type<'v>(
        this: &'v StarlarkTargetNode,
        heap: Heap<'v>,
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
    ///     target_node = ctx.uquery().eval("//foo:bar")[0]
    ///     ctx.output.print(target_node.rule_kind)
    /// ```
    #[starlark(attribute)]
    fn rule_kind<'v>(
        this: &'v StarlarkTargetNode,
        heap: Heap<'v>,
    ) -> starlark::Result<StringValue<'v>> {
        Ok(heap.alloc_str_intern(this.0.rule_kind().as_str()))
    }

    /// Gets the target's special attr `oncall`
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_get_oncall(ctx):
    ///     target_node = ctx.uquery().eval("//foo:bar")[0]
    ///     ctx.output.print(target_node.oncall)
    /// ```
    #[starlark(attribute)]
    fn oncall<'v>(
        this: &'v StarlarkTargetNode,
        heap: Heap<'v>,
    ) -> starlark::Result<NoneOr<StringValue<'v>>> {
        match this.0.oncall() {
            None => Ok(NoneOr::None),
            Some(oncall) => Ok(NoneOr::Other(heap.alloc_str_intern(oncall))),
        }
    }

    /// Gets all deps for this target.
    /// The result is a list of `UnconfiguredTargetLabel`.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_get_deps(ctx):
    ///     target_node = ctx.uquery().eval("//foo:bar")[0]
    ///     ctx.output.print(target_node.deps())
    /// ```
    fn deps<'v>(
        this: &'v StarlarkTargetNode,
    ) -> starlark::Result<AllocList<impl IntoIterator<Item = StarlarkTargetLabel> + 'v>> {
        Ok(AllocList(
            this.0
                .deps()
                .map(|label| StarlarkTargetLabel::new(label.dupe())),
        ))
    }

    /// Gets all files which are an immediate input to the rule function and thus are needed to go through analysis.
    /// The result is a list of `CellPath`.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_get_deps(ctx):
    ///     target_node = ctx.uquery().eval("//foo:bar")[0]
    ///     ctx.output.print(target_node.inputs())
    /// ```
    fn inputs<'v>(
        this: &'v StarlarkTargetNode,
    ) -> starlark::Result<AllocList<impl IntoIterator<Item = StarlarkCellPath> + 'v>> {
        Ok(AllocList(this.0.inputs().map(StarlarkCellPath)))
    }
}
