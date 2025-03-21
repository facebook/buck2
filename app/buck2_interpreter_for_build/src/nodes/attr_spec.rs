/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under both the MIT license found in the
 * LICENSE-MIT file in the root directory of this source tree and the Apache
 * License, Version 2.0 found in the LICENSE-APACHE file in the root directory
 * of this source tree.
 */

use std::collections::HashMap;

use buck2_core::target::label::label::TargetLabelRef;
use buck2_core::target::name::TargetNameRef;
use buck2_error::internal_error;
use buck2_error::BuckErrorContext;
use buck2_node::attrs::attr::Attribute;
use buck2_node::attrs::attr::CoercedValue;
use buck2_node::attrs::attr_type::string::StringLiteral;
use buck2_node::attrs::coerced_attr::CoercedAttr;
use buck2_node::attrs::configurable::AttrIsConfigurable;
use buck2_node::attrs::inspect_options::AttrInspectOptions;
use buck2_node::attrs::spec::internal::attr_is_configurable;
use buck2_node::attrs::spec::internal::NAME_ATTRIBUTE;
use buck2_node::attrs::spec::internal::VISIBILITY_ATTRIBUTE;
use buck2_node::attrs::spec::internal::WITHIN_VIEW_ATTRIBUTE;
use buck2_node::attrs::spec::AttributeId;
use buck2_node::attrs::spec::AttributeSpec;
use buck2_node::attrs::values::AttrValues;
use buck2_util::arc_str::ArcStr;
use dupe::Dupe;
use starlark::docs::DocString;
use starlark::eval::ParametersParser;
use starlark::eval::ParametersSpec;
use starlark::eval::ParametersSpecParam;
use starlark::typing::ParamIsRequired;
use starlark::typing::ParamSpec;
use starlark::typing::Ty;
use starlark::typing::TyFunction;
use starlark::values::Value;

use crate::attrs::AttributeCoerceExt;
use crate::interpreter::module_internals::ModuleInternals;
use crate::nodes::check_within_view::check_within_view;

pub trait AttributeSpecExt {
    fn start_parse<'a, 'v>(
        &'a self,
        param_parser: &mut ParametersParser<'v, '_>,
        size_hint: usize,
    ) -> buck2_error::Result<(
        // "name" attribute value.
        &'v TargetNameRef,
        // Remaining attributes.
        impl ExactSizeIterator<Item = (&'a str, AttributeId, &'a Attribute)> + 'a,
        // Populated with name.
        AttrValues,
    )>;

    fn parse_params<'v>(
        &self,
        param_parser: &mut ParametersParser<'v, '_>,
        arg_count: usize,
        internals: &ModuleInternals,
    ) -> buck2_error::Result<(&'v TargetNameRef, AttrValues)>;

    /// Returns a starlark Parameters for the rule callable.
    fn signature(&self, rule_name: String) -> ParametersSpec<Value<'_>>;

    fn ty_function(&self) -> TyFunction;

    fn starlark_types(&self) -> Vec<Ty>;
    fn docstrings(&self) -> HashMap<String, Option<DocString>>;
}

impl AttributeSpecExt for AttributeSpec {
    fn start_parse<'a, 'v>(
        &'a self,
        param_parser: &mut ParametersParser<'v, '_>,
        size_hint: usize,
    ) -> buck2_error::Result<(
        &'v TargetNameRef,
        impl ExactSizeIterator<Item = (&'a str, AttributeId, &'a Attribute)> + 'a,
        AttrValues,
    )> {
        let mut attr_values = AttrValues::with_capacity(size_hint);

        let mut indices = self.attr_specs();
        let name = match indices.next() {
            Some((name_name, attr_idx, _attr)) if name_name == NAME_ATTRIBUTE.name => {
                let name = param_parser.next()?;
                attr_values.push_sorted(
                    attr_idx,
                    CoercedAttr::String(StringLiteral(ArcStr::from(name))),
                );
                name
            }
            _ => {
                return Err(internal_error!("First attribute is `name`, it is known"));
            }
        };
        let name = TargetNameRef::new(name)?;
        Ok((name, indices, attr_values))
    }

    /// Parses params extracting the TargetName and the attribute values to store in the TargetNode.
    fn parse_params<'v>(
        &self,
        param_parser: &mut ParametersParser<'v, '_>,
        arg_count: usize,
        internals: &ModuleInternals,
    ) -> buck2_error::Result<(&'v TargetNameRef, AttrValues)> {
        let (name, indices, mut attr_values) = self.start_parse(param_parser, arg_count)?;

        let target_label = TargetLabelRef::new(internals.buildfile_path().package(), name);

        for (attr_name, attr_idx, attribute) in indices {
            let configurable = attr_is_configurable(attr_name);

            let user_value: Option<Value> = match attribute.default() {
                Some(_) => param_parser.next_opt()?,
                None => Some(param_parser.next()?),
            };

            let attr_is_visibility = attr_name == VISIBILITY_ATTRIBUTE.name;
            let attr_is_within_view = attr_name == WITHIN_VIEW_ATTRIBUTE.name;
            if let Some(v) = user_value {
                let mut coerced = attribute
                    .coerce(
                        attr_name,
                        configurable,
                        internals.attr_coercion_context(),
                        v,
                    )
                    .with_buck_error_context(|| {
                        format!(
                            "Error coercing attribute `{}` of `{}`",
                            attr_name, target_label,
                        )
                    })?;

                if attr_is_visibility {
                    if coerced == CoercedValue::Default {
                        coerced = CoercedValue::Custom(CoercedAttr::Visibility(
                            internals.super_package.visibility().dupe(),
                        ));
                    }
                } else if attr_is_within_view {
                    if coerced == CoercedValue::Default {
                        coerced = CoercedValue::Custom(CoercedAttr::WithinView(
                            internals.super_package.within_view().dupe(),
                        ));
                    }
                }

                match coerced {
                    CoercedValue::Custom(v) => {
                        attr_values.push_sorted(attr_idx, v);
                    }
                    CoercedValue::Default => {}
                }
            } else if attr_is_visibility {
                attr_values.push_sorted(
                    attr_idx,
                    CoercedAttr::Visibility(internals.super_package.visibility().dupe()),
                );
            } else if attr_is_within_view {
                attr_values.push_sorted(
                    attr_idx,
                    CoercedAttr::WithinView(internals.super_package.within_view().dupe()),
                );
            }
        }

        attr_values.shrink_to_fit();

        // For now `within_view` is always set, but let's make code more robust.
        if let Some(within_view) = attr_values.get(WITHIN_VIEW_ATTRIBUTE.id) {
            let within_view = match within_view {
                CoercedAttr::WithinView(within_view) => within_view,
                _ => return Err(internal_error!("`within_view` coerced incorrectly")),
            };
            for a in self.attrs(&attr_values, AttrInspectOptions::DefinedOnly) {
                check_within_view(
                    a.value,
                    internals.buildfile_path().package(),
                    a.attr.coercer(),
                    within_view,
                )
                .with_buck_error_context(|| {
                    format!(
                        "checking `within_view` for attribute `{}` of `{}`",
                        a.name, target_label,
                    )
                })?;
            }
        }

        Ok((name, attr_values))
    }

    /// Returns a starlark Parameters for the rule callable.
    fn signature(&self, rule_name: String) -> ParametersSpec<Value<'_>> {
        ParametersSpec::new_named_only(
            &rule_name,
            self.attr_specs().map(|(name, _idx, attribute)| {
                let default = attribute.default();
                (
                    name,
                    match default {
                        Some(_) => ParametersSpecParam::Optional,
                        None => ParametersSpecParam::Required,
                    },
                )
            }),
        )
    }

    fn ty_function(&self) -> TyFunction {
        let mut params = Vec::with_capacity(self.attr_specs().len());
        for (name, _idx, attribute) in self.attr_specs() {
            let ty = match attr_is_configurable(name) {
                AttrIsConfigurable::Yes => attribute.starlark_type().to_ty_with_select(),
                AttrIsConfigurable::No => attribute.starlark_type().to_ty(),
            };
            let required = match attribute.default() {
                Some(_) => ParamIsRequired::No,
                None => ParamIsRequired::Yes,
            };
            params.push((starlark::util::ArcStr::from(name), required, ty));
        }
        let params = ParamSpec::new_named_only(params).unwrap();
        TyFunction::new(params, Ty::none())
    }

    fn starlark_types(&self) -> Vec<Ty> {
        self.attr_specs()
            .map(|(_, _, a)| a.starlark_type().to_ty())
            .collect()
    }

    fn docstrings(&self) -> HashMap<String, Option<DocString>> {
        self.attr_specs()
            .map(|(name, _idx, attr)| (name.to_owned(), attr.docstring()))
            .collect()
    }
}
