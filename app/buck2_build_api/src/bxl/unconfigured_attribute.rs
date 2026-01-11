/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is dual-licensed under either the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree or the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree. You may select, at your option, one of the
 * above-listed licenses.
 */

use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::source_artifact::SourceArtifact;
use buck2_core::package::PackageLabel;
use buck2_core::package::source_path::SourcePath;
use buck2_error::starlark_error::from_starlark_with_options;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use buck2_interpreter::types::configured_providers_label::StarlarkProvidersLabel;
use buck2_interpreter::types::target_label::StarlarkTargetLabel;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::display::AttrDisplayWithContext;
use buck2_node::attrs::fmt_context::AttrFmtContext;
use buck2_node::attrs::serialize::AttrSerializeWithContext;
use buck2_node::visibility::VisibilityPatternList;
use buck2_node::visibility::VisibilitySpecification;
use buck2_node::visibility::WithinViewSpecification;
use derive_more::From;
use dupe::Dupe;
use gazebo::prelude::SliceExt;
use serde::Serialize;
use starlark::__derive_refs::serde::Serializer;
use starlark::any::ProvidesStaticType;
use starlark::environment::Methods;
use starlark::environment::MethodsBuilder;
use starlark::environment::MethodsStatic;
use starlark::starlark_module;
use starlark::starlark_simple_value;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark::values::dict::Dict;
use starlark::values::list::AllocList;
use starlark::values::starlark_value;
use starlark::values::tuple::AllocTuple;
use starlark_map::small_map::SmallMap;

use crate::bxl::select::StarlarkSelectConcat;
use crate::bxl::select::StarlarkSelectDict;
use crate::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;

#[derive(Debug, ProvidesStaticType, From, Allocative)]
pub struct StarlarkCoercedAttr(pub CoercedAttr, pub PackageLabel);

starlark_simple_value!(StarlarkCoercedAttr);

impl Display for StarlarkCoercedAttr {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(
            &AttrFmtContext {
                package: Some(self.1.dupe()),
                options: Default::default(),
            },
            f,
        )
    }
}

impl Serialize for StarlarkCoercedAttr {
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

/// Coerced attr from an unconfigured target node.
#[starlark_value(type = "CoercedAttr")]
impl<'v> StarlarkValue<'v> for StarlarkCoercedAttr {
    fn get_methods() -> Option<&'static Methods> {
        static RES: MethodsStatic = MethodsStatic::new();
        RES.methods(coerced_attr_methods)
    }
}

/// Methods on configured target node's attributes.
#[starlark_module]
fn coerced_attr_methods(builder: &mut MethodsBuilder) {
    /// Returns the type name of the attribute
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_type(ctx):
    ///     node = ctx.uquery().owner("bin/TARGETS")[0]
    ///     ctx.output.print(node.attrs.name.type)
    /// ```
    // FIXME(JakobDegen): Strings as types are mostly dead, users should be getting the value and
    // using `isinstance` instead. Remove this.
    #[starlark(attribute)]
    fn r#type<'v>(this: &StarlarkCoercedAttr, heap: Heap<'v>) -> starlark::Result<&'v str> {
        Ok(this.0.to_value(this.1.dupe(), heap)?.get_type())
    }

    /// Returns the value of this attribute. Limited support of selects, concats, and explicit configuration deps
    /// at this time.
    ///
    /// Sample usage:
    /// ```python
    /// def _impl_value(ctx):
    ///     node = ctx.uquery().owner("bin/TARGETS")[0]
    ///     ctx.output.print(node.attrs.name.value())
    /// ```
    fn value<'v>(this: &StarlarkCoercedAttr, heap: Heap<'v>) -> starlark::Result<Value<'v>> {
        Ok(this.0.to_value(this.1.dupe(), heap)?)
    }
}

pub trait CoercedAttrExt {
    fn to_value<'v>(&self, pkg: PackageLabel, heap: Heap<'v>) -> buck2_error::Result<Value<'v>>;
}

impl CoercedAttrExt for CoercedAttr {
    /// Converts the coerced attr to a starlark value
    fn to_value<'v>(&self, pkg: PackageLabel, heap: Heap<'v>) -> buck2_error::Result<Value<'v>> {
        Ok(match &self {
            CoercedAttr::Bool(v) => heap.alloc(v.0),
            CoercedAttr::Int(v) => heap.alloc(*v),
            CoercedAttr::String(s) | CoercedAttr::EnumVariant(s) => heap.alloc(s.as_str()),
            CoercedAttr::List(list) => heap.alloc(list.try_map(|v| v.to_value(pkg.dupe(), heap))?),
            CoercedAttr::Tuple(v) => {
                heap.alloc(AllocTuple(v.try_map(|v| v.to_value(pkg.dupe(), heap))?))
            }
            CoercedAttr::Dict(map) => {
                let mut res = SmallMap::with_capacity(map.len());

                for (k, v) in map.iter() {
                    res.insert_hashed(
                        k.to_value(pkg.dupe(), heap)?.get_hashed().map_err(|e| {
                            from_starlark_with_options(
                                e,
                                buck2_error::starlark_error::NativeErrorHandling::Unknown,
                                false,
                            )
                        })?,
                        v.to_value(pkg.dupe(), heap)?,
                    );
                }

                heap.alloc(Dict::new(res))
            }
            CoercedAttr::None => Value::new_none(),
            CoercedAttr::OneOf(l, _) => l.as_ref().to_value(pkg, heap)?,
            CoercedAttr::Visibility(VisibilitySpecification(specs))
            | CoercedAttr::WithinView(WithinViewSpecification(specs)) => match specs {
                VisibilityPatternList::Public => heap.alloc(AllocList(["PUBLIC"])),
                VisibilityPatternList::List(specs) => {
                    heap.alloc(AllocList(specs.iter().map(|s| s.to_string())))
                }
            },
            CoercedAttr::ExplicitConfiguredDep(d) => heap.alloc(
                // TODO(@wendyy) - this needs better support
                StarlarkProvidersLabel::new(d.as_ref().label.dupe()),
            ),
            CoercedAttr::ConfiguredDepForForwardNode(d) => heap.alloc(
                StarlarkConfiguredProvidersLabel::new(d.as_ref().label.dupe()),
            ),
            CoercedAttr::TransitionDep(d) => {
                let label = StarlarkProvidersLabel::new(d.dep.dupe());
                match d.get_dynamic_transition() {
                    Some(t) => heap.alloc((label, StarlarkProvidersLabel::new(t.dupe()))),
                    None => heap.alloc(label),
                }
            }
            CoercedAttr::SplitTransitionDep(d) => heap.alloc(StarlarkProvidersLabel::new(d.dupe())),
            CoercedAttr::ConfigurationDep(c) => {
                // TODO(T198210718)
                heap.alloc(StarlarkTargetLabel::new(c.target().dupe()))
            }
            CoercedAttr::PluginDep(d) => heap.alloc(StarlarkTargetLabel::new(d.dupe())),
            CoercedAttr::Dep(d) => heap.alloc(StarlarkProvidersLabel::new(d.dupe())),
            CoercedAttr::SourceLabel(s) => heap.alloc(StarlarkProvidersLabel::new(s.dupe())),
            CoercedAttr::Label(l) => heap.alloc(StarlarkProvidersLabel::new(l.dupe())),
            CoercedAttr::Arg(arg) => heap.alloc(arg.to_string()),
            CoercedAttr::Query(query) => heap.alloc(&query.query.query),
            CoercedAttr::SourceFile(f) => heap.alloc(StarlarkArtifact::new(Artifact::from(
                SourceArtifact::new(SourcePath::new(pkg.to_owned(), f.path().dupe())),
            ))),
            CoercedAttr::Metadata(data) => heap.alloc(data.to_value()),
            CoercedAttr::TargetModifiers(data) => heap.alloc(data.to_value()),
            CoercedAttr::Selector(selector) => {
                let select_dict = StarlarkSelectDict::new(*selector.clone(), pkg.dupe());
                heap.alloc(select_dict)
            }
            CoercedAttr::Concat(c) => heap.alloc(StarlarkSelectConcat::new(c.clone(), pkg.dupe())),
        })
    }
}
