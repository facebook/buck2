/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::fmt::Display;
use std::fmt::Formatter;

use allocative::Allocative;
use buck2_artifact::artifact::artifact_type::Artifact;
use buck2_artifact::artifact::source_artifact::SourceArtifact;
use buck2_build_api::interpreter::rule_defs::artifact::starlark_artifact::StarlarkArtifact;
use buck2_build_api::interpreter::rule_defs::provider::dependency::DependencyGen;
use buck2_core::package::source_path::SourcePath;
use buck2_core::package::PackageLabel;
use buck2_error::starlark_error::from_starlark_with_options;
use buck2_interpreter::types::configured_providers_label::StarlarkConfiguredProvidersLabel;
use buck2_interpreter::types::configured_providers_label::StarlarkProvidersLabel;
use buck2_interpreter::types::opaque_metadata::OpaqueMetadata;
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
use starlark::values::dict::Dict;
use starlark::values::list::AllocList;
use starlark::values::list::ListRef;
use starlark::values::none::NoneType;
use starlark::values::starlark_value;
use starlark::values::tuple::AllocTuple;
use starlark::values::FrozenValue;
use starlark::values::Heap;
use starlark::values::StarlarkValue;
use starlark::values::Value;
use starlark_map::small_map::SmallMap;

use crate::bxl::starlark_defs::select::StarlarkSelectConcat;
use crate::bxl::starlark_defs::select::StarlarkSelectDict;

#[derive(Debug, ProvidesStaticType, From, Allocative)]
pub(crate) struct StarlarkCoercedAttr(pub(crate) CoercedAttr, pub(crate) PackageLabel);

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
#[starlark_value(type = "coerced_attr")]
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
    #[starlark(attribute)]
    fn r#type<'v>(this: &StarlarkCoercedAttr) -> starlark::Result<&'v str> {
        Ok(this.0.starlark_type()?)
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
    fn value<'v>(this: &StarlarkCoercedAttr, heap: &'v Heap) -> starlark::Result<Value<'v>> {
        Ok(this.0.to_value(this.1.dupe(), heap)?)
    }
}

pub(crate) trait CoercedAttrExt {
    fn starlark_type(&self) -> buck2_error::Result<&'static str>;

    fn to_value<'v>(&self, pkg: PackageLabel, heap: &'v Heap) -> buck2_error::Result<Value<'v>>;
}

impl CoercedAttrExt for CoercedAttr {
    /// Returns the starlark type of this attr
    fn starlark_type(&self) -> buck2_error::Result<&'static str> {
        match self {
            CoercedAttr::Bool(_) => Ok(starlark::values::bool::BOOL_TYPE),
            CoercedAttr::Int(_) => Ok(starlark::values::int::INT_TYPE),
            CoercedAttr::String(_) | CoercedAttr::EnumVariant(_) => {
                Ok(starlark::values::string::STRING_TYPE)
            }
            CoercedAttr::List(_) => Ok(starlark::values::list::ListRef::TYPE),
            CoercedAttr::Tuple(_) => Ok(starlark::values::tuple::TupleRef::TYPE),
            CoercedAttr::Dict(_) => Ok(Dict::TYPE),
            CoercedAttr::None => Ok(NoneType::TYPE),
            CoercedAttr::OneOf(l, _) => l.as_ref().starlark_type(),
            CoercedAttr::Visibility(..) => Ok(ListRef::TYPE),
            CoercedAttr::WithinView(..) => Ok(ListRef::TYPE),
            CoercedAttr::ExplicitConfiguredDep(_) => {
                Ok(DependencyGen::<FrozenValue>::get_type_value_static().as_str())
            }
            CoercedAttr::SplitTransitionDep(_) => Ok(Dict::TYPE),
            CoercedAttr::ConfigurationDep(_) => Ok(starlark::values::string::STRING_TYPE),
            CoercedAttr::PluginDep(_) => Ok(StarlarkTargetLabel::get_type_value_static().as_str()),
            CoercedAttr::Dep(_) => Ok(StarlarkProvidersLabel::get_type_value_static().as_str()),
            CoercedAttr::SourceLabel(_) => {
                Ok(StarlarkProvidersLabel::get_type_value_static().as_str())
            }
            CoercedAttr::Label(_) => Ok(StarlarkProvidersLabel::get_type_value_static().as_str()),
            CoercedAttr::Arg(_) => Ok(starlark::values::string::STRING_TYPE),
            CoercedAttr::Query(_) => Ok(starlark::values::string::STRING_TYPE),
            CoercedAttr::SourceFile(_) => Ok(StarlarkArtifact::get_type_value_static().as_str()),
            CoercedAttr::Metadata(..) => Ok(OpaqueMetadata::get_type_value_static().as_str()),
            CoercedAttr::TargetModifiers(..) => {
                Ok(OpaqueMetadata::get_type_value_static().as_str())
            }
            CoercedAttr::Selector(_) => Ok("SelectorDict"),
            // TODO(@wendyy) - starlark concat is not implemented.
            CoercedAttr::Concat(_) => Ok("concat"),
            CoercedAttr::ConfiguredDep(_) => {
                Ok(StarlarkConfiguredProvidersLabel::get_type_value_static().as_str())
            }
        }
    }

    /// Converts the coerced attr to a starlark value
    fn to_value<'v>(&self, pkg: PackageLabel, heap: &'v Heap) -> buck2_error::Result<Value<'v>> {
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
            CoercedAttr::ConfiguredDep(d) => heap.alloc(StarlarkConfiguredProvidersLabel::new(
                d.as_ref().label.dupe(),
            )),
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
